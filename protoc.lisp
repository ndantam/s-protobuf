;; Copyright 2009, Georgia Tech Research Corporation
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

;;;; This is damn ugly, and I should really do a better job.
;;;;
;;;; FIXME: remove symbol pollution via gensysms 
;;;;  (not a catastrophic unless library users are doing strange things 
;;;;  with internal symbols in package protocol-buffer-compiler
;;;;
;;;; FIXME: Instead of "directly" generating so much code, 
;;;;   move some pieces into macros and then just generate calls to those.  
;;;;   That should make this process significantly easier to understand.


(defpackage :protocol-buffer-compiler
  (:nicknames :protoc)
  (:export :def-proto-msg :def-proto-enum :load-proto-set)
  (:use :cl))

(in-package :protocol-buffer-compiler)

(defparameter *message-plist-sym* 'message)

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



(defparameter +types+ '(:double :float
                        :int32 :int64
                        :uint32 :uint64
                        :sint32 :sint64
                        :fixed32 :fixed64
                        :sfixed32 :sfixed64
                        :bool :string :bytes))

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
    ((:bool :int32 :sint32 :uint32 :int64 :sint64 :enum :int32 :uint64)
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

(defun declare-message (raw-form &optional (package *package*))
  (destructuring-bind (message name &rest specs) raw-form
    (let ((msg-name-sym (pb-sym name package)))
      (setf (get msg-name-sym *message-plist-sym*)
            `(,message ,(pb-sym name package)
                      ,@(loop for spec in specs
                           collect 
                             (destructuring-bind (field name type &rest keys)
                                 spec
                               `(,field ,(pb-sym name package) 
                                       ,type ,@keys)))))
      msg-name-sym)))


(defun symbol-string= (a b)
  (string= (string a) (string b)))


(defun make-start-code-sym (slot-position type)
  (pb::make-start-code slot-position (wire-typecode type)))

;(defun slot-default-value (type repeated default)
  ;(cond
    ;(default default)
    ;(repeated (make-array 0 :elment-type

(defun gen-pack1 (bufsym startsym valsym type)
  `(incf ,startsym
         ,(case type
                ((:int32 :uint32 :uint64 :enum)
                 `(binio:encode-uvarint ,valsym ,bufsym ,startsym))
                (:bool
                 `(pb::encode-bool ,valsym 
                                   ,bufsym 
                                   ,startsym))
                ((:sint32 :sint64)
                 `(binio:encode-svarint ,valsym ,bufsym ,startsym))
                ((:fixed32 :sfixed32)
                 `(binio:encode-int ,valsym :little ,bufsym ,startsym 32))
                ((:fixed64 :sfixed64)
                 `(binio:encode-int ,valsym :little ,bufsym ,startsym 64))
                (:string
                 (let ((strbuf (gensym))
                       (size (gensym)))
                   `(multiple-value-bind (,size ,strbuf)
                        (binio:encode-utf8 ,valsym)
                      (incf ,startsym 
                            (binio:encode-uvarint ,size ,bufsym ,startsym))
                      (replace ,bufsym ,strbuf :start1 ,startsym)
                      ,size)))
                (otherwise ;; pack object
                 (if (enum-type-p type)
                     `(binio:encode-uvarint (,(symcat type 'code) ,valsym)
                                            ,bufsym ,startsym)
                     `(pb::pack-embedded ,valsym ,bufsym ,startsym))))))


(defun gen-start-code-size (type pos)
  (binio:uvarint-size (make-start-code-sym pos type)))

(defun gen-scalar-size (type slot pos)
  `(+ ,(gen-start-code-size type pos)
      ,(cond 
        ((fixed64-p type) 8)
        ((fixed32-p type) 4)
        ((eq :bool type) 1)
        ((uvarint-p type)
         `(binio:uvarint-size ,slot))
        ((svarint-p type) 
         `(binio:svarint-size ,slot))
        ((enum-type-p type) 
         `(binio:uvarint-size (,(symcat type 'code) ,slot)))
        ((eq :string type)
         `(pb::length-delim-size (binio:utf8-size ,slot)))
        ((eq :bytes type)
         `(pb::length-delim-size (length ,slot)))
        (t `(pb::length-delim-size (pb::packed-size ,slot))))))

(defun gen-repeated-size (type slot pos)
  (cond 
    ((fixed-p type) 
     `(* (length ,slot)
         (+ ,(gen-start-code-size type pos) 
            ,(fixed-size type))))
    (t 
     (let ((i (gensym))
           (accum (gensym)))
       `(let ((,accum 0))
          (dotimes (,i (length ,slot))
            (incf ,accum 
                  ,(gen-scalar-size type `(aref ,slot ,i) pos)))
          ,accum)))))

        ;((pb::svarint-p type) 
         ;`(pb::packed-uvarint-size ,slot))
        ;(t `(pb::packed-size ,slot))

(defun gen-packed-size (type slot &optional pos)
  (let ((array-size 
         (cond 
           ((fixed64-p type) `(* 8 (length ,slot)))
           ((fixed32-p type) `(* 4 (length ,slot)))
           ((eq :bool type) `(length ,slot))
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

(defun gen-slot-size (type objsym slot-name pos packed repeated)
  (let ((slot `(slot-value ,objsym ',slot-name)))
    (cond
      ((and (not repeated) (not packed))
       (gen-scalar-size type slot pos))
      ((and repeated (not packed))
       (gen-repeated-size type slot pos))
      (packed
       (gen-packed-size type slot pos)))))

  
(defun def-packed-size (form package)
  (destructuring-bind (message name &rest field-specs) form
    (assert (symbol-string= message 'message) () "Not a message form")
    (let ((protobuf (pb-sym 'protobuf package)))
      `(defmethod pb:packed-size ((,protobuf ,name))
         (+ ,@(mapcan (lambda (field-spec)
                        (when (symbol-string= (car field-spec) "FIELD")
                          (destructuring-bind (field name type position 
                                                     &key 
                                                     (default nil)
                                                     (required nil)
                                                     (repeated nil)
                                                     (optional nil)
                                                     (packed nil))
                              field-spec
                            (declare (ignore field default required optional))
                            (list (gen-slot-size type protobuf  name position 
                                                 packed repeated))
                            )))
                      field-specs))))))
 
         
(defun gen-pack-slot (bufsym startsym objsym name pos type repeated packed)
  (let ((slot `(slot-value ,objsym ',name)))
    (cond 
      ;; scalar value
      ((null repeated)
       `( ;; write start code
         (incf ,startsym 
               (pb::encode-start-code ,pos 
                                      ,(wire-typecode type)
                                      ,bufsym ,startsym))

         ;; write data code
         ,(gen-pack1 bufsym startsym slot type)))
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
             ,(gen-pack1 bufsym startsym  `(aref ,slot ,countsym) type)))))
      ;; repeated value
      ((and repeated packed)
       `( ;; write start code
         (incf ,startsym 
               (pb::encode-start-code ,pos 
                                      ,(wire-typecode :bytes)
                                      ,bufsym ,startsym))
         ;; write length
         ,(gen-pack1 bufsym startsym 
                     (gen-packed-size type slot) :uint64)
         ;; write elements
         ,(let ((isym (gensym)))
               `(dotimes (,isym (length ,slot))
                  ,(gen-pack1 bufsym startsym 
                              `(aref ,slot ,isym) type))))))))
         

(defun msg-defpack ( form package)
  (destructuring-bind (message name &rest field-specs) form
    (assert (symbol-string= message 'message) () "Not a message form")
    (let ((protobuf (pb-sym 'protobuf package))
          (buffer (pb-sym 'buffer package))
          (start (pb-sym 'start package))
          (i (gensym)))
      `(defmethod pb:pack ((,protobuf ,name)
                           &optional
                           (,buffer (binio:make-octet-vector 
                                     (pb:packed-size ,protobuf)))
                           (,start 0))
         (let ((,i ,start))
           ,@(mapcan (lambda (field-spec)
                       (when (symbol-string= (car field-spec) "FIELD")
                         (destructuring-bind (field name type position 
                                                    &key 
                                                    (default nil)
                                                    (required nil)
                                                    (repeated nil)
                                                    (optional nil)
                                                    (packed nil))
                             field-spec
                           (declare (ignore field default required optional))
                           (gen-pack-slot buffer i protobuf 
                                          name position type repeated packed)
                           )))
                        field-specs)
           (values (- ,i ,start) ,buffer))))))

(defun get-decoder-name (protobuf-type)
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
    (:bool
     'pb::decode-bool)
    (otherwise 
     (if (enum-type-p protobuf-type)
         (symcat protobuf-type 'decode)
         (error "Can't handle this type: ~A" protobuf-type)))))

(defun gen-unpack1 (bufsym startsym type placesym)
      `(pb::with-decoding (value length)
           ,(cond 
             ((primitive-type-p type)
              `(,(get-decoder-name type)
                 ,bufsym ,startsym))
             (t `(pb::unpack-embedded-protobuf ,bufsym ,placesym ,startsym)))
         (incf ,startsym length)
         value))

(defun gen-unpacker (bufsym startsym objsym name type repeated packed)
  (let ((slot  `(slot-value ,objsym ',name)))
    (cond 
      ((and (not repeated) (not packed))
       `(
         ;(format t "~&unpacking scalar")
         ,@(unless (primitive-type-p type)
                   `((unless (slot-boundp ,objsym ',name)
                       ;(format t "~&making new slot")
                       (setf ,slot (make-instance ',(lisp-type type))))))
         (setf ,slot ,(gen-unpack1 bufsym startsym  type slot))))
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
      ((and repeated (not packed))
       `((unless (slot-boundp ,objsym ',name)
           (setf ,slot 
                 (make-array 0 :element-type ',(lisp-type type) 
                             :fill-pointer t)))
         (vector-push-extend ,(gen-unpack1 bufsym startsym type
                                           (if (primitive-type-p type) nil
                                               `(make-instance ',type)))
                             ,slot)))
      (t (error "can't handle this type")))))

(defun def-unpack (form package)
  (declare (ignore package))
  (destructuring-bind (message name &rest field-specs) form
    (declare (ignore message))
    `(defmethod pb:unpack (buffer
                            (protobuf ,name)
                            &optional (start 0) (end (length buffer)))
       (declare (binio:octet-vector buffer))
       (do ((i start))
           ((>= i end) (values protobuf (- i start)))
         (multiple-value-bind (pos typecode startlen)
             (pb::read-start-code buffer i)
           (incf i startlen)
           (case pos
             ,@(mapcan (lambda (field-spec)
                        (when (symbol-string= (car field-spec) "FIELD")
                          (destructuring-bind (field name type position 
                                                     &key 
                                                     (default nil)
                                                     (required nil)
                                                     (repeated nil)
                                                     (packed nil)
                                                     (optional nil))
                              field-spec
                            (declare (ignore field default required optional))
                            (let ((desired-typecode (wire-typecode type repeated packed)))
                              `((,position 
                                 (assert 
                                  (= typecode 
                                     ,desired-typecode) ()
                                     "Invalid typecode for field ~A. Wanted ~A (~A) but found ~A (~A)." 
                                     (quote ,name) 
                                     ,desired-typecode (pb:typecode-meaning ,desired-typecode)
                                     typecode (pb:typecode-meaning typecode))
                                 ,@(gen-unpacker 'buffer 'i 'protobuf name type repeated packed)
                                 ))))))
                       field-specs)
             (otherwise (error "Unhandled position ~A in class ~A, buffer ~A, need to skip" 
                               pos ',name buffer))))))))


(defun gen-init-form (type repeated packed)
  (cond 
    ((and (not repeated) (not packed))
     (cond ((integer-type-p type)  0)
           ((eq type :string) nil)
           ((eq type :bool) nil)
           ((eq type :double) 0d0)
           ((eq type :float) 0s0)
           ((enum-type-p type) nil)
           (t `(make-instance ',type))))
    ((and repeated packed)
     nil)
    ((and repeated (not packed))
     `(make-array 0 :element-type ',(lisp-type type) :fill-pointer t))
    (t (error "Cant make init form for packed, nonrepeated elements"))))
          
    
(defun gen-enum (enum-name enums)
  (setf (get enum-name 'enum) t)
  (labels ((map-enums (function enums)
             (mapcar (lambda (e) (funcall function (car e) (cadr e))) enums)))
    (let ((sym (gensym))
          (code (gensym))
          (buffer (gensym))
          (start (gensym))
          (value (gensym))
          (length (gensym))
          (enum-symbol (symcat enum-name 'symbol)))
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
              (values (,enum-symbol ,value) ,length)))))))
        
 

;;    `((defmethod pb::enum-symbol ((enum-name (eql ',enum-name)) (enum-code integer))
;;        (case enum-code
;;          ,@(map-enums (lambda (symbol code) `(,code ,symbol)) enums)
;;          (otherwise (error "Unknown enum ~A code: ~A" ',enum-name enum-code))))
;;      (defmethod pb::enum-code ((enum-name (eql ',enum-name)) (enum-symbol symbol))
;;        (case enum-symbol
;;          ,@(map-enums (lambda (symbol code) `(,symbol ,code)) enums)
;;          (otherwise (error "Unknown enum ~A symbol: ~A" ',enum-name enum-symbol)))))))
        
  
(defun msg-def-enums (msg-name specs)
  (mapcan (lambda (spec)
            (when (symbol-string= (car spec) 'enum)
              (gen-enum (symcat (symbol-package msg-name) 
                                msg-name (cadr spec))
                        (cddr spec))))
          specs))

(defun msg-defclass (name field-specs)
  `((cl:defclass ,name () ())
    (cl:defclass ,name ()
      ;; slots
      ,(mapcan (lambda (field-spec)
                 (when (symbol-string= (car field-spec) "FIELD")
                   (destructuring-bind (field field-name type position 
                                              &key 
                                              (default nil)
                                              (required nil)
                                              (repeated nil)
                                              (packed nil)
                                              (optional nil))
                       field-spec
                     (declare (ignore position field packed
                                      default required optional))
                     `((,field-name 
                        :type ,(lisp-type type repeated)
                        ;:initform ,(gen-init-form type repeated packed)
                        )))))
               field-specs))))



(defun load-proto-se (path &optional (package (find-package :cl-user)))
  (with-open-file (stream path)
    (loop 
       for form = (read stream nil)
       until (null form)
       do
         (format t "~S~%" form)
       collect
         (cond 
           ((symbol-string= (car form) "MESSAGE")
            (format t "Declareing: ~&~S~%" form)
            (declare-message form package))
           (t (error "Unknown form in proto file: ~A" (car form)))))))


(defun gen-msg-defs (name body)
  (let ((form `(message ,name ,@body)))
    `(
       ,@(msg-def-enums name body) 
       ,@(msg-defclass name body)
       ,(def-packed-size form (symbol-package name))
       ,(msg-defpack form (symbol-package name))
       ,(def-unpack form (symbol-package name)))))

(defmacro def-proto-msg (name &body body)
    `(progn
       ,@(gen-msg-defs name body)))


(defmacro def-proto-enum (name &body values)
  `(progn
     ,@(gen-enum name values)))
          
