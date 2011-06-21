;; Copyright 2009-2010, Georgia Tech Research Corporation
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; * Redistributions of source code must retain the above copyright
;;   notice, this list of conditions and the following disclaimer.
;;
;; * Redistributions in binary form must reproduce the above copyright
;;   notice, this list of conditions and the following disclaimer in
;;   the documentation and/or other materials provided with the
;;   distribution.
;;
;; * Neither the name of the copyright holder(s) nor the names of its
;;   contributors may be used to endorse or promote products derived
;;   from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.


;;;; Protocol Buffer Compiler for CL
;;;; Author: Neil T. Dantam

;;;; FIXME: Instead of "directly" generating so much code, 
;;;;   move some pieces into macros and then just generate calls to those.  
;;;;   That should make this process somewhat easier to understand.


;;;;;;;;;;;;;;;;;;;;
;;; PACKAGE ;;;
;;;;;;;;;;;;;;;;;;;;

(defpackage :protocol-buffer-compiler
  (:nicknames :protoc)
  (:export :def-proto-msg :def-proto-enum :load-proto-set)
  (:use :cl))

(in-package :protocol-buffer-compiler)


;;;;;;;;;;;;;;;;;;;;
;;; TYPE QUERIES ;;;
;;;;;;;;;;;;;;;;;;;;

(defparameter +types+ '(:double :float
                        :int32 :int64
                        :uint32 :uint64
                        :sint32 :sint64
                        :fixed32 :fixed64
                        :sfixed32 :sfixed64
                        :boolean :bool :string :bytes))

(defun enum-type-p (sym)
  (get sym 'enum))

(defun primitive-type-p (type)
  (or
   (find type +types+ :test #'eq)
   (enum-type-p type)))

(defun fixed64-p (type)
  (case type
    ((:double :fixed64 :sfixed64) t)
    (otherwise nil)))

(defun fixed32-p (type)
  (case type
    ((:float :sfixed32 :fixed32) t)
    (otherwise nil)))

(defun fixed-p (type)
  (or (fixed32-p type)
      (fixed64-p type)))

(defun varint-p (type)
  (case type
    ((:boolean :bool :int32 :sint32 :uint32 :int64 :sint64 :uint64 :enum)
     t)
    (otherwise nil )))

(defun varint-enum-p (type)
  (or (varint-p type)
      (enum-type-p type)))

(defun svarint-p (type)
  (case type
    ((:sint32 :sint64) t)
    (otherwise nil)))

(defun uvarint-p (type)
  (and (varint-p type) (not (svarint-p type))))

(defun integer-type-p (type)
  (or (varint-enum-p type) (fixed-p type)))

(defun length-delim-p (type)
  (and (not (fixed64-p type))
       (not (fixed32-p type))
       (not (varint-p type))))

(defun fixed-size (type)
  (cond ((fixed32-p type) 4)
        ((fixed64-p type) 8)
        (t (error "Not a fixed type: ~A" type))))

(defun wire-typecode (type &optional repeated packed)
  (if (and repeated packed) 2
      (cond 
        ((varint-enum-p type) 0)
        ((fixed64-p type) 1)
        ((fixed32-p type) 5)
        ((length-delim-p type) 2)
        (t 2))))

(defun lisp-type (ident &optional repeated)
  (let ((base (case ident
                ((:int32 :sfixed32 :sint32) '(cl:signed-byte 32))
                ((:uint32 :fixed32) '(cl:unsigned-byte 32))
                ((:int64 :sfixed64 :sint64) '(cl:signed-byte 64))
                ((:uint64 :fixed64) '(cl:unsigned-byte 64))
                ((:bool) 'cl:boolean)
                ((:boolean) 'cl:boolean)
                (:double 'cl:double-float)
                (:string 'cl:string)
                (:float 'cl:single-float)
                (otherwise 
                 (if (enum-type-p ident)
                     'cl:symbol 
                     ident)))))
    (if repeated 
        `(array ,base *)
        base)))


;;;;;;;;;;;;;;;;;;;;;;;
;;; NAME CONVERSION ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun symcat (package-or-sym &rest symbols)
  (intern (reduce (lambda (a b) 
                    (concatenate 'string (string a) "-" (string b)))
                  (if (symbolp package-or-sym) 
                      (cons package-or-sym symbols)
                      symbols))
          (cond 
            ((packagep package-or-sym) package-or-sym)
            ((symbolp package-or-sym) (symbol-package package-or-sym))
            (t (error "Can't find package for ~A" package-or-sym)))))

(defun mangle-upcase-hypen (ident)
  (with-output-to-string (s)
    (let* ((str (string ident))
           (uncamel (find-if #'lower-case-p str)))
      (princ (char-upcase (aref str 0)) s)
      (loop for c across (subseq str 1)
         do (cond 
              ((and uncamel (upper-case-p c))
               (princ #\- s)
               (princ  c s))
              ((lower-case-p c)
               (princ (char-upcase c) s))
              ((eq c #\_)
               (princ #\- s))
              (t
               (princ c s)))))))
                               
(defun pb-str (name)
  (mangle-upcase-hypen name))

(defun pb-sym (name &optional package)
  (if package
      (intern (pb-str name) 
              package))
      (intern (pb-str name) ))

(defun make-start-code-sym (slot-position type)
  (pb::make-start-code slot-position (wire-typecode type)))

(defun symbol-keyword (sym)
  (intern (symbol-name sym) :keyword))

(defun symbol-string= (a b)
  (string= (string a) (string b)))


;;;;;;;;;;;;;
;;; UTILS ;;;
;;;;;;;;;;;;;

(defmacro with-gensyms ( (&rest symbols) &body body )
  `(let ,(mapcar (lambda (sym)
                   `(,sym (gensym)))
                 symbols)
     ,@body))

(defun map-fields (function specs &rest args)
  (mapcan (lambda (field-spec)
            (when (symbol-string= (car field-spec) "FIELD")
              (list (apply function (append args (cdr field-spec))))))
          specs))


;;;;;;;;;;;;
;;; SIZE ;;;
;;;;;;;;;;;;

(defun gen-start-code-size (type pos)
  "Find the size of the start code"
  (binio:uvarint-size (make-start-code-sym pos type)))

(defmacro def-scalar-size (type slot pos)
  "Packed size of a scalar field."
  `(+ ,(gen-start-code-size type pos)
      ,(cond 
        ((fixed64-p type) 8)
        ((fixed32-p type) 4)
        ((eq :bool type) 1)
        ((eq :boolean type) 1)
        ((uvarint-p type)
         `(binio:uvarint-size ,slot))
        ((svarint-p type) 
         `(binio:svarint-size ,slot))
        ((enum-type-p type) 
         `(binio:uvarint-size (,(symcat type 'code) ,slot)))
        ((eq :string type)
         `(if ,slot (pb::length-delim-size (binio:utf8-size ,slot))
              0))
        ((eq :bytes type)
         `(if ,slot (pb::length-delim-size (length ,slot))
              0))
        (t `(if ,slot (pb::length-delim-size (pb::packed-size ,slot))
                0)))))

(defmacro def-repeated-size (type slot pos)
  "Packed size of a repeated field."
  (if (fixed-p type) 
      `(* (length ,slot)
          (+ ,(gen-start-code-size type pos) 
             ,(fixed-size type)))
      (with-gensyms (i accum)
        `(let ((,accum 0))
           (dotimes (,i (length ,slot))
             (incf ,accum 
                   (def-scalar-size ,type (aref ,slot ,i) ,pos)))
           ,accum))))

(defmacro def-repeated-packed-size (type slot &optional pos)
  "Packed size of a repeated packed field."
  (let ((array-size 
         (cond 
           ((fixed64-p type) `(* 8 (length ,slot)))
           ((fixed32-p type) `(* 4 (length ,slot)))
           ((eq :bool type) `(length ,slot))
           ((eq :boolean type) `(length ,slot))
           ((uvarint-p type) 
            `(pb::packed-uvarint-size ,slot))
           ((svarint-p type) 
            `(pb::packed-uvarint-size ,slot))
           ((enum-type-p type) 
            `(pb::packed-enum-size #',(symcat type 'code) ,slot))
           (t (error "Can't pack this type")))))
    (if pos
        `(+ ,(gen-start-code-size :bytes pos) 
            (pb::length-delim-size ,array-size))
        array-size)))

(defun gen-slot-size (object slot-name type pos &key 
                      packed repeated &allow-other-keys)
  "Generate code to find the packed size of a single slot."
  (let* ((objsym (gensym))
         (slot  `(slot-value ,objsym ',slot-name)))
    `(let ((,objsym ,object))
       (if (and (slot-boundp ,objsym ',slot-name)
                ,(if (length-delim-p type)
                     slot
                     t))
           ,(cond
             ((and (not repeated) (not packed))
              `(def-scalar-size ,type ,slot ,pos))
             ((and repeated (not packed))
              `(def-repeated-size ,type ,slot ,pos))
             (packed
              `(def-repeated-packed-size ,type ,slot ,pos)))
           0))))
  
(defun gen-packed-size (name specs)
  "Generate code for the PACKED-SIZE defmethod."
  (with-gensyms (protobuf)
    `(defmethod pb:packed-size ((,protobuf ,name))
       (+ ,@(map-fields #'gen-slot-size specs protobuf)))))


;;;;;;;;;;;;;;;
;;; PACKING ;;;
;;;;;;;;;;;;;;;

(defmacro def-pack-val (start-place buffer slot type)
  "Generate code to pack a single value into the buffer."
  (with-gensyms (buffer-sym slot-sym)
    `(incf ,start-place
           (let ((,buffer-sym ,buffer)
                 (,slot-sym ,slot))
             ,(case type
                    ((:int32 :uint32 :uint64 :int64 :enum)
                     `(binio:encode-uvarint ,slot-sym ,buffer-sym ,start-place))
                    ((:bool :boolean)
                     `(pb::encode-bool ,slot-sym 
                                       ,buffer-sym 
                                       ,start-place))
                    ((:sint32 :sint64)
                     `(binio:encode-svarint ,slot-sym ,buffer-sym ,start-place))
                    ((:fixed32 :sfixed32)
                     `(binio:encode-int ,slot-sym :little ,buffer-sym ,start-place 32))
                    ((:fixed64 :sfixed64)
                     `(binio:encode-int ,slot-sym :little ,buffer-sym ,start-place 64))
                    ((:double)
                     `(binio:encode-double-float
                       ,slot-sym :little ,buffer-sym ,start-place))
                    ((:float)
                     `(binio:encode-single-float
                       ,slot-sym :little ,buffer-sym ,start-place))
                    (:string
                     (with-gensyms (strbuf size)
                       `(multiple-value-bind (,size ,strbuf)
                            (binio:encode-utf8 ,slot-sym)
                          (incf ,start-place 
                                (binio:encode-uvarint ,size ,buffer-sym ,start-place))
                          (replace ,buffer-sym ,strbuf :start1 ,start-place)
                          ,size)))
                    (otherwise ;; pack object
                     (if (enum-type-p type)
                         `(binio:encode-uvarint (,(symcat type 'code) ,slot-sym)
                                                ,buffer-sym ,start-place)
                         `(pb::pack-embedded ,slot-sym ,buffer-sym ,start-place)
                              )))))))
           
(defun gen-pack-slot (bufsym startsym objsym name type pos &key 
                      repeated packed &allow-other-keys)
  "Generate code to pack a single slot."
  (let ((slot `(slot-value ,objsym ',name)))
    `(when (and (slot-boundp ,objsym ',name)
                ,(if (length-delim-p type)
                     slot
                     t))
      ,@(cond 
         ;; scalar value
         ((null repeated)
          `( ;; write start code
            (incf ,startsym 
                  (pb::encode-start-code ,pos 
                                         ,(wire-typecode type)
                                         ,bufsym ,startsym))

            ;; write data code
            (def-pack-val ,startsym ,bufsym ,slot ,type)))
         ;; repeated unpacked value
         ((and repeated (not packed))
          (let ((countsym (gensym)))
            `((dotimes (,countsym (length ,slot)) ; n times
                ;; write start code
                (incf ,startsym 
                      (pb::encode-start-code ,pos 
                                             ,(wire-typecode type)
                                             ,bufsym ,startsym))
                ;; write element

                (def-pack-val ,startsym ,bufsym (aref ,slot ,countsym) ,type)))))
         ;; repeated value
         ((and repeated packed)
          `( ;; write start code
            (incf ,startsym 
                  (pb::encode-start-code ,pos 
                                         ,(wire-typecode :bytes)
                                         ,bufsym ,startsym))
            ;; write length
            (def-pack-val ,startsym ,bufsym (def-repeated-packed-size ,type ,slot) :uint64)
            ;; write elements
            ,(let ((isym (gensym)))
                  `(dotimes (,isym (length ,slot))
                     (def-pack-val ,startsym ,bufsym (aref ,slot ,isym) ,type))))))
      )))

(defun gen-pack (name specs)
  "Generate code for the PACK defmethod."
  (with-gensyms (protobuf buffer start i)
    `(defmethod pb:pack ((,protobuf ,name)
                         &optional
                         (,buffer (binio:make-octet-vector 
                                   (pb:packed-size ,protobuf)))
                         (,start 0))
       (declare (type binio:octet-vector ,buffer)
                (fixnum ,start))
       (let ((,i ,start))
         ,@(map-fields #'gen-pack-slot specs
                       buffer i protobuf)
         (values (- ,i ,start) ,buffer)))))


;;;;;;;;;;;;;;;;;
;;; UNPACKING ;;; 
;;;;;;;;;;;;;;;;;

(defun get-decoder-name (protobuf-type)
  "Find the function symbol to decode this type."
  (case protobuf-type
    ((:int32 :uint32 :int64 :uint64 :enum)
     'binio:decode-uvarint)
    ((:sint32 :sint64)
     'binio:decode-svarint)
    ((:fixed32)
     'pb::decode-uint32)
    ((:sfixed32)
     'pb::decode-sint32)
    ((:fixed64 )
     'pb::decode-uint64)
    ((:sfixed64)
     'pb::decode-sint64)
    (:string
     'pb::decode-string)
    (:double
     'pb::decode-double)
    (:float
     'pb::decode-single)
    ((:bool :boolean)
     'pb::decode-bool)
    (otherwise 
     (if (enum-type-p protobuf-type)
         (symcat protobuf-type 'decode)
         (error "Can't find decoder for this type: ~A" protobuf-type)))))

(defmacro do-unpack-value (type buffer start &optional instance)
  (if (primitive-type-p type)
      `(,(get-decoder-name type) ,buffer ,start)
      (progn
        (assert instance () "Need instance to unpack embedded message ~A" type)
        `(pb::unpack-embedded-protobuf ,buffer ,instance ,start))))

(defmacro do-unpack-and-incf (start-place buffer type &optional instance)
  "Unpack a scalar value and increment start."
  (with-gensyms (value length)
    `(pb::with-decoding (,value ,length)
         (do-unpack-value ,type ,buffer ,start-place ,instance)
       ,@(when (primitive-type-p type)
               `((declare (type ,(lisp-type type) ,value))))
       (incf ,start-place ,length)
       ,value)))

;; FIXME: should split this function up
(defun gen-unpacker (bufsym startsym objsym name type repeated packed)
  "Generate code to unpack a single slot"
  (let ((slot  `(slot-value ,objsym ',name)))
    (cond 
      ;; scalar slot
      ((and (not repeated) (not packed))
       `( ,@(unless (primitive-type-p type)
                    `((unless (and (slot-boundp ,objsym ',name)
                                   ,slot)
                        (setf ,slot (make-instance ',(lisp-type type))))))
            (setf ,slot 
                  (do-unpack-and-incf ,startsym ,bufsym ,type ,slot))))
      ;; packed array
      ((and repeated packed)
       `((pb::with-decoding (value length)
             (pb::decode-length-delim ,bufsym ,startsym 
                                      (lambda (buffer start end)
                                        (pb::decode-array ',(lisp-type type)
                                                          #',(get-decoder-name type)
                                                          buffer
                                                          :fixed-bit-size 
                                                          ,(when (fixed-p type)
                                                                 (* 8 (fixed-size 
                                                                       type)))
                                                          :start start
                                                          :end end)))
           (setf ,slot value)
           (incf ,startsym length))))
      ;; array, not packed 
      ((and repeated (not packed))
       ;; FIXME: SBCL conses here because it's not a simple array
       ;; would be nice to avoid that
       `((vector-push-extend 
          (do-unpack-and-incf ,startsym ,bufsym ,type 
                              ,(when (not (primitive-type-p type))
                                     `(make-instance ',type)))
          ,slot)))
      ;; something else
      (t (error "Can't make unpacker for this type ~A, repeated: ~A, packed: ~A" 
                type repeated packed)))))

(defun gen-unpack (name specs)
  "Generate code for the UNPACK method"
  (with-gensyms (buffer protobuf start end i pos typecode startlen)
    `(defmethod pb:unpack (,buffer
                           (,protobuf ,name)
                           &optional (,start 0) (,end (length ,buffer)))
       (declare (type binio:octet-vector ,buffer)
                (fixnum ,start ,end))
       ;; loop through buffer until we at the end
       ;; each decoded field will INCF i by its length
       (do ((,i ,start))
           ((>= ,i ,end) (values ,protobuf (the fixnum (- ,i ,start))))
         (declare (fixnum ,i))
         (multiple-value-bind (,pos ,typecode ,startlen)
             (pb::read-start-code ,buffer ,i)
           (declare (fixnum ,pos ,typecode ,startlen))
           (incf ,i ,startlen)
           ;; current working with a simple case statement,
           ;; perhaps I could do something more efficient...
           (case ,pos
             ,@(map-fields (lambda (name type position &key repeated packed 
                               &allow-other-keys)
                             (let ((desired-typecode (wire-typecode type repeated packed)))
                               `(,position 
                                 (assert 
                                  (= ,typecode ,desired-typecode) ()
                                  "Invalid typecode for field ~A. Wanted ~A (~A) but found ~A (~A)." 
                                  (quote ,name) 
                                  ,desired-typecode (pb:typecode-meaning ,desired-typecode)
                                  ,typecode (pb:typecode-meaning ,typecode))
                                 ,@(gen-unpacker buffer i protobuf name type repeated packed)
                                 )))
                           specs)
             (otherwise (error "Unhandled position ~A in class ~A, buffer ~A, need to skip" 
                               ,pos ',name ,buffer))))))))


;;;;;;;;;;;;;
;;; ENUMS ;;;
;;;;;;;;;;;;;

(defun gen-enum (enum-name enums)
  "Generate code to map between enum codes and keyword symbols."
  (setf (get enum-name 'enum) t)
  (labels ((map-enums (function enums)
             (mapcar (lambda (e) (funcall function (car e) (cadr e))) enums)))
    (with-gensyms (sym code buffer start value length)
      (let ((enum-symbol (symcat enum-name 'symbol)))
        `((defun ,enum-symbol (,code)
            (case ,code
              ,@(map-enums (lambda (symbol code) `(,code ,symbol)) enums)
              (otherwise (error "Unknown enum ~A code: ~A" ',enum-name ,code))))
          (defun ,(symcat enum-name 'code) (,sym)
            (case ,sym
              ,@(map-enums (lambda (symbol code) `(,symbol ,code)) enums)
              (otherwise (error "Unknown enum ~A symbol: ~A" ',enum-name ,sym))))
          (defun ,(symcat enum-name 'decode) (,buffer ,start)
            (pb::with-decoding (,value ,length)
                (binio:decode-uvarint ,buffer ,start)
              (values (,enum-symbol ,value) ,length))))))))
        
(defun gen-internal-enums (name specs)
  (mapcan (lambda (spec)
            (when (symbol-string= (car spec) 'enum)
              (gen-enum (symcat (symbol-package name) 
                                name (cadr spec))
                        (cddr spec))))
          specs))


;;;;;;;;;;;;;;;
;;; CLASSES ;;;
;;;;;;;;;;;;;;;

(defun gen-init-form (type repeated packed)
  "Generate the value to set a field to on MAKE-INSTANCE"
  (cond 
    ((and (not repeated) (not packed))
     ;; Scalar types
     (cond ((eq type :bool) nil)
           ((eq type :boolean) nil)
           ((eq type :double) 0d0)
           ((eq type :float) 0s0)
           ((integer-type-p type)  0)
           ((eq type :string) nil)
           ((enum-type-p type) nil)
           (t nil)))
    ((and repeated packed) 
     ;; packed array
     nil)
    ((and repeated (not packed)) 
     ;; not packed array
     `(make-array 0 :element-type ',(lisp-type type) :fill-pointer t :adjustable t))
    (t (error "Cant make init form for packed, nonrepeated elements"))))

(defun gen-class (class-name specs)
  "Generate code for the DEFCLASS."
  `((cl:defclass ,class-name () ())
    (cl:defclass ,class-name ()
      ;; slots
      ,(map-fields (lambda (name type position &key repeated packed
                       &allow-other-keys)
                     (declare (ignore position))
                     `(,name 
                       :type ,(lisp-type type repeated)
                       :initarg ,(symbol-keyword name)
                       :initform ,(gen-init-form type repeated packed)
                       ))
                   specs))))


;;;;;;;;;;;;;;;;;;;;
;;; AGGREGRATION ;;;
;;;;;;;;;;;;;;;;;;;;

(defun gen-msg-defs (name specs)
  "Generate all definitions for one message type"
  `(,@(gen-internal-enums name specs)
      ,@(gen-class name specs)
      ,(gen-packed-size name specs)
      ,(gen-pack name specs)
      ,(gen-unpack name specs)))

(defmacro def-proto-msg (name &body specs)
    `(progn
       ,@(gen-msg-defs name specs)))

(defmacro def-proto-enum (name &body values)
  `(progn
     ,@(gen-enum name values)))
          
