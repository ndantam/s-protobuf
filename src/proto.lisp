;; Copyright 2009, Georgia Tech Research Corporation
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:

;;     * Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above
;; copyright notice, this list of conditions and the following disclaimer
;; in the documentation and/or other materials provided with the
;; distribution.
;;     * Neither the name of Google Inc. nor the names of its
;; contributors may be used to endorse or promote products derived from
;; this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; Runtime support functions for protocol buffers
;; Author Neil T. Dantam

;(declaim (optimize (speed 3) (safety 0)))

(defpackage :protocol-buffer
  (:nicknames :pb)
  (:use :cl :binio)
  (:export
   :pack :pack1 :encode
   :unpack
   :packed-size
   :enum-symbol
   :enum-code
   :protocol-buffer
   :typecode-meaning))

(in-package :protocol-buffer)

;; generic function acessors
(defgeneric pack (protobuf &optional buf start))
(defgeneric packed-size (protobuf))
(defgeneric unpack (buffer protobuf &optional start end))

(defgeneric enum-symbol (enum-name enum-code))
(defgeneric enum-code (enum-name enum-symbol))

(defun pack1 (protobuf)
  (multiple-value-bind (size buf)
      (pack protobuf)
    (declare (ignore size))
    buf))

(defun encode (protobuf)
  (multiple-value-bind (size buf)
      (pack protobuf)
    (values buf size)))
;;(defparameter +types+ '(:double :float
;;                        :int32 :int64
;;                        :uint32 :uint64
;;                        :sint32 :sint64
;;                        :fixed32 :fixed64
;;                        :sfixed32 :sfixed64
;;                        :bool :string :bytes))

;; encoders return (values bytes-encoded buffer)
;; decoders return (values value bytes-decoded)

;; types
(deftype start-code () '(integer 0 5))


;;(defun enum-type-p (sym)
  ;;(get sym 'protoc::enum))

;;(defun primitive-type-p (type)
  ;;(or
   ;;(find type +types+ :test #'eq)
   ;;(enum-type-p type)))

;;(defun fixed64-p (type)
;;  (case type
;;    ((:double :fixed64 :sfixed64) t)
;;    (otherwise nil)))

;;(defun fixed32-p (type)
;;  (case type
;;    ((:float :sfixed32 :fixed32) t)
;;    (otherwise nil)))


;;(defun fixed-p (type)
;;  (or (fixed32-p type)
;;      (fixed64-p type)))

;;(defun varint-p (type)
;;  (case type
;;    ((:bool :int32 :sint32 :uint32 :int64 :sint64 :enum :int32 :uint64)
;;     t)
;;    (otherwise nil )))

;;(defun varint-enum-p (type)
;;  (or (varint-p type)
;;      (enum-type-p type)))

;;(defun svarint-p (type)
;;  (case type
;;    ((:sint32 :sint64) t)
;;    (otherwise nil)))

;;(defun uvarint-p (type)
;;  (and (varint-p type) (not (svarint-p type))))

;;(defun integer-type-p (type)
;;  (or (varint-enum-p type) (fixed-p type)))

;;(defun length-delim-p (type)
;;  (and (not (fixed64-p type))
;;       (not (fixed32-p type))
;;       (not (varint-p type))))

;;(defun fixed-size (type)
;;  (cond ((fixed32-p type) 4)
;;        ((fixed64-p type) 8)
;;        (t (error "Not a fixed type: ~A" type))))

;;(defun wire-typecode (type &optional repeated packed)
;;  (if (and repeated packed) 2
;;      (cond 
;;        ((varint-enum-p type) 0)
;;        ((fixed64-p type) 1)
;;        ((fixed32-p type) 5)
;;        ((length-delim-p type) 2)
;;        (t 2))))

;;; runtime support functions

;;; All decoding functions return (values object bytes-decoded)

(declaim (inline slot-bound-setp))
(defun slot-bound-setp (object slot-name)
  (and (slot-boundp object slot-name)
       (slot-value  object slot-name)))

;; make it obvious what we're doing
(defmacro with-decoding ((value length) decode-expr &body body)
  `(multiple-value-bind (,value ,length)
       ,decode-expr
     (declare (fixnum ,length))
     ,@body))

(defun make-start-code (slot-position typecode)
  (declare (type fixnum slot-position typecode))
  (logior (ash slot-position 3) typecode))

(declaim (inline read-start-code))
(defun read-start-code (buffer start)
  (declare (type octet-vector buffer)
           (fixnum start))
  "returns (values position typecode bytes-read)"
  (with-decoding (vi i) (decode-uvarint buffer start)
    (declare (type fixnum vi i))
    (values (ash vi -3) (ldb (byte 3 0) vi) i)))

(defun encode-start-code (slot-position typecode buffer start)
  (declare (type octet-vector buffer)
           (fixnum start typecode slot-position))
  (encode-uvarint (make-start-code slot-position typecode)
                 buffer start))

(defun pack-embedded (protobuf buffer start)
  (declare (type octet-vector buffer)
           (fixnum start))
  (let* ((size (packed-size protobuf))
         (size-size (binio:encode-uvarint size buffer start))
         (encoded-size (pack protobuf buffer (+ start size-size))))
    (declare (fixnum size size-size encoded-size))
    (assert (= size encoded-size))
    (values (+ size-size encoded-size) buffer)))
    

(defun packed-uvarint-size (array)
  (declare (type (simple-array integer (*)) array))
  (loop for x across array 
     summing (binio::uvarint-size x)))

(defun packed-svarint-size (array)
  (declare (type (simple-array integer (*)) array))
  (loop for x across array 
     summing (binio::svarint-size x)))

(defun packed-enum-size (coder array)
  (loop for x across array 
     summing (binio::uvarint-size (funcall coder x))))

(defun length-delim-size (length)
  (declare (fixnum length))
  (+ (binio::uvarint-size length) length))

(defun typecode-meaning (typecode)
  (ecase typecode
    (0 :varint)
    (1 :fixed64)
    (5 :fixed32)
    (2 :size-delimited)))

;;; encoders
(defun encode-bool (val buffer start)
  (declare (type octet-vector buffer)
           (fixnum start))
  (setf (aref buffer start) (if val 1 0))
  1)

;;; fixed-width decoders
(declaim (inline decode-uint32
                 decode-sint32
                 decode-uint64
                 decode-sint64
                 decode-double
                 decode-bool))
(defun decode-uint32 (buffer start)
  (values (binio:decode-uint32-le buffer start)
          4))

(defun decode-sint32 (buffer start)
  (values (binio:decode-sint32-le buffer start) 
          4))

(defun decode-uint64 (buffer start)
  (values (binio:decode-uint64-le buffer start)
          8))

(defun decode-sint64 (buffer start)
  (values (binio:decode-sint64-le buffer start)
          8))

(defun decode-double (buffer start)
  (values (binio:decode-double-float-le buffer start)
          ;(binio:decode-double-float buffer :little start)
          8))


(defun decode-single (buffer start)
  (values (binio:decode-float-le buffer start)
          4))

(defun decode-bool (buffer start)
  (declare (type octet-vector buffer)
           (fixnum start))
  (values (case (aref buffer start)
            (0 nil)
            (1 t)
            (otherwise 
             (error "Invalid boolean valude")))
          1))
        

(defmacro decode-length-and-incf-start (start-place buffer)
  "reads the length field, 
increments start-place by length,
returns (values length length-of-length)"
  (let ((isym (gensym))
        (len-sym (gensym))
        (len-len-sym (gensym)))
    `(let ((,isym ,start-place))
       (with-decoding (,len-sym ,len-len-sym)
           (binio:decode-uvarint ,buffer ,isym)
         (setf ,start-place (+ ,isym ,len-len-sym))
         (values ,len-sym ,len-len-sym)))))
         
(defun decode-length-delim (buffer start decoder)
  "decoder is (lambda (buffer start end)"
  (declare (type octet-vector buffer)
           (integer start))
  (let ((i start))
    (with-decoding (len len-len)
        (decode-length-and-incf-start i buffer)
      (with-decoding (val val-len)
          (funcall decoder buffer i (+ i len))
        (assert (= val-len len))
        (values val (+ len len-len))))))

(defun decode-string (buffer start)
  (declare (type octet-vector buffer)
           (integer start))
  (decode-length-delim buffer start 
                       (lambda (buffer start end)
                         (binio::decode-utf8 buffer 
                                             :buffer-start start 
                                             :buffer-end end))))

(defun unpack-embedded-protobuf (buffer protobuf start)
  (declare (type octet-vector buffer)
           (integer start))
  (decode-length-delim buffer start
                       (lambda (buffer start end)
                         (unpack buffer protobuf start end))))


(defun decode-array (type decoder buffer &key 
                     (fixed-bit-size nil) 
                     (start 0) 
                     end )
  (declare (type octet-vector buffer)
           (integer start))
  (assert (or (not fixed-bit-size) 
              (zerop (rem fixed-bit-size 8))) ()
              "Can only decode integral-octet-sized types")
  (let ((end (or end (length buffer))))
    (let* ((array (make-array (if fixed-bit-size
                                  (/ (- end start)
                                     (/ fixed-bit-size 8))
                                  0)
                              :element-type type
                              :adjustable (not fixed-bit-size)
                              :fill-pointer (not fixed-bit-size))))
      (do ((i start)
           (j 0 (1+ j)))
          ((>= i end) (values array (- i start)))
        (multiple-value-bind (value length)
            (funcall decoder buffer i)
          (incf i (if fixed-bit-size (/ fixed-bit-size 8) length))
          (if fixed-bit-size
              (setf (aref array j) value)
              (vector-push-extend value array)))))))
                     

;;(defun proto-test ()
;;  (labels ((test-start-code (type pos result)
;;             (let ((buffer (make-octet-vector 1)))
;;               (encode-start-code pos (wire-typecode type) buffer 0)
;;               (equalp buffer (octet-vector result)))))
;;    ;; from the google docs
;;    (assert (test-start-code :int32 1 #16r08))
;;    (assert (test-start-code :string 2 #16r12))
;;    t))
