;; Copyright (c) 2009, Georgia Tech Research Corporation
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


;; Binary encoding
;; Author: Neil T. Dantam

(defpackage :binio
  (:use :cl)
  (:export 
   :octet :octet-vector :make-octet-vector
   :decode-uint :decode-sint 
   :encode-int 
   :encode-double-float :decode-double-float
   :encode-single-float :decode-single-float
   :encode-svarint :decode-svarint
   :encode-uvarint :decode-uvarint
   :uvarint-size :svarint-size
   :make-octet-vector :octet-vector
   :encode-utf8 :decode-utf8
   :utf8-size
   :decode-double-float-le :decode-double-float-be
   :decode-float-le :decode-float-be
   :decode-uint32-le :decode-uint32-be
   :decode-sint32-le :decode-sint32-be
   :decode-uint64-le :decode-uint64-be
   :decode-sint64-le :decode-sint64-be
   ))

;; types u?int{8,16,32,63}, double, float


;;(declaim (optimize (speed 3) (safety 0)))
(in-package :binio)

;; encoding fuctions:
;;  (value &optional buffer start) => (values bytes-encoded buffer)
;; decoding fuctions:
;;  (buffer &optional start) => (values value bytes-decoded)

;;;;;;;;;;;;;
;;; types ;;;
;;;;;;;;;;;;;

(deftype octet () '(unsigned-byte 8))
(deftype octet-vector (&optional count)
  `(simple-array octet (,count)))

(declaim (inline make-octet-vector))
(defun make-octet-vector (count)
  (declare (fixnum count))
  (make-array count :element-type 'octet))

(defun octet-vector (&rest args)
  (let ((v (make-octet-vector (length args))))
    (loop 
       for x in args
       for i from 0
       do
         (setf (aref v i) x))
    v))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; some endian handling ;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (declaim (inline index-endian aref-endian (setf aref-endian)))
;; (defun index-endian (index start count endian)
;;   (declare (fixnum start count index)
;;            (type symbol endian))
;;   (case endian
;;     (:little 
;;      (+ start index)) 
;;     (:big 
;;      (+ start count -1 (- index)))
;;     (otherwise 
;;      (error "endian must be :big or :little, not ~S" endian))))

;; (defun aref-endian (buffer index start count endian)
;;   (declare (fixnum start count index))
;;   (declare (type octet-vector buffer))
;;   (aref buffer (index-endian index start count endian)))

;; (defun (setf aref-endian) (value buffer index start count endian)
;;   (declare (fixnum start count index))
;;   (declare (type octet-vector buffer))
;;   (setf (aref buffer (index-endian index start count endian))
;;         value))


;;;;;;;;;;;;;;;;;;;;;
;;; CFFI DECODERS ;;;
;;;;;;;;;;;;;;;;;;;;;

;; FIXME: this CFFI hack is much faster than letting lisp clumsily
;; screw with bignums.  Everything should get converted to use this
;; eventually.

(eval-when (:compile-toplevel :load-toplevel)
  (defun needs-byteswap (endian)
    (assert (or (eq :little endian)
                (eq :big endian)))
    (let ((native-endian (ecase (cffi:with-foreign-object (x :uint16)
                                  (setf (cffi:mem-aref x :uint8 0) 1)
                                  (setf (cffi:mem-aref x :uint8 1) 0)
                                  (cffi:mem-ref x :uint16))
                           (1 :little)
                           (256 :big))))
      (not (eq endian native-endian)))))

;; A fast path for SBCL when native and encoded are the same
;; Question: Do we need to pin the buffer?
#+sbcl
(defmacro def-decoder-sbcl (c-type)
  `(progn
     (assert (>= (length buffer)
                 (+ start ,(cffi:foreign-type-size c-type)))
             () "Buffer too small for requested data type: ~A" ,c-type)
     (cffi:mem-ref (cffi:inc-pointer (sb-sys:vector-sap buffer) start)
                   ,c-type)))

(defmacro def-decoder-cffi (c-type swap)
  `(progn
     (cffi:with-foreign-object (x ,c-type)
       (setf ,@(loop 
                  with n = (cffi:foreign-type-size c-type)
                  for i below n
                  for j = (if swap 
                              (- n i 1)
                              i)
                  append
                    `((cffi:mem-aref x :uint8 ,j) 
                      (aref buffer (+ start ,i)))))
       (cffi:mem-ref x ,c-type))))

(defmacro def-decoder (name c-type lisp-type endian)
  `(progn
     (declaim (inline ,name))
     (defun ,name (buffer &optional (start 0))
       (declare (type octet-vector buffer)
                (type fixnum start))
       (the ,lisp-type
         ,(let ((swap (needs-byteswap endian)))
               (if (and (not swap) nil 
                        (string= "SBCL" (lisp-implementation-type)))
                   `(def-decoder-sbcl ,c-type)
                   `(def-decoder-cffi ,c-type ,swap)))))))
  
;; Note: swapping the byte-order is about 5 times slower
(def-decoder decode-double-float-le :double double-float :little)
(def-decoder decode-double-float-be :double double-float :big)

(def-decoder decode-float-le :float single-float :little)
(def-decoder decode-float-be :float single-float :big)

(def-decoder decode-sint64-le :int64 (signed-byte 64) :little)
(def-decoder decode-sint64-be :int64 (signed-byte 64) :big)

(def-decoder decode-uint64-le :uint64 (unsigned-byte 64) :little)
(def-decoder decode-uint64-be :uint64 (unsigned-byte 64) :big)

(def-decoder decode-sint32-le :int32 (signed-byte 32) :little)
(def-decoder decode-sint32-be :int32 (signed-byte 32) :big)

(def-decoder decode-uint32-le :uint32 (unsigned-byte 32) :little)
(def-decoder decode-uint32-be :uint32 (unsigned-byte 32) :big)

(def-decoder decode-sint16-le :int16 (signed-byte 16) :little)
(def-decoder decode-sint16-be :int16 (signed-byte 16) :big)

(def-decoder decode-uint16-le :uint16 (unsigned-byte 16) :little)
(def-decoder decode-uint16-be :uint16 (unsigned-byte 16) :big)


;;;;;;;;;;;;;;;;;;;;;
;;; CFFI ENCODERS ;;;
;;;;;;;;;;;;;;;;;;;;;

;; A fast path for SBCL when native and encoded are the same
;; Question: Do we need to pin the buffer?
#+sbcl
(defmacro def-encoder-sbcl (c-type)
  `(progn  
     (assert (>= (length buffer)
                 (+ start ,(cffi:foreign-type-size c-type)))
             () "Buffer too small for requested data type: ~A" ,c-type)
     (setf (cffi:mem-ref (cffi:inc-pointer (sb-sys:vector-sap buffer) 
                                           start)
                         ,c-type)
           value)))


(defmacro def-encoder-cffi (c-type swap)
  `(cffi:with-foreign-object (x ,c-type)
     (setf (cffi:mem-ref x ,c-type) value)
     (setf ,@(loop 
                with n = (cffi:foreign-type-size c-type)
                for i below n
                for j = (if swap  
                            (- n i 1)
                            i)
                append
                  `((aref buffer (+ start ,i))
                    (cffi:mem-aref x :uint8 ,j))))
     (cffi:mem-ref x ,c-type)))

(defmacro def-encoder (name c-type lisp-type endian)
  `(progn
     (declaim (inline ,name))
     (defun ,name (value &optional 
                   (buffer (make-octet-vector ,(cffi:foreign-type-size c-type)))
                   (start 0))
       (declare (type ,lisp-type value)
                (type octet-vector buffer)
                (type fixnum start))
       ,(let ((swap (needs-byteswap endian)))
             (if (and (not swap)
                      (string= "SBCL" (lisp-implementation-type)))
                 `(def-encoder-sbcl ,c-type)
                 `(def-encoder-cffi ,c-type ,swap)))
       (values ,(cffi:foreign-type-size c-type) buffer))))


(def-encoder encode-double-le :double double-float :little)
(def-encoder encode-double-be :double double-float :big)
(def-encoder encode-float-le :float single-float :little)
(def-encoder encode-float-be :float single-float :big)

(def-encoder encode-uint64-le :uint64 (unsigned-byte 64) :little)
(def-encoder encode-uint64-be :uint64 (unsigned-byte 64) :big)
(def-encoder encode-sint64-le :int64 (signed-byte 64) :little)
(def-encoder encode-sint64-be :int64 (signed-byte 64) :big)

(def-encoder encode-uint32-le :uint32 (unsigned-byte 32) :little)
(def-encoder encode-uint32-be :uint32 (unsigned-byte 32) :big)
(def-encoder encode-sint32-le :int32 (signed-byte 32) :little)
(def-encoder encode-sint32-be :int32 (signed-byte 32) :big)

(def-encoder encode-uint16-le :uint16 (unsigned-byte 16) :little)
(def-encoder encode-uint16-be :uint16 (unsigned-byte 16) :big)
(def-encoder encode-sint16-le :int16 (signed-byte 16) :little)
(def-encoder encode-sint16-be :int16 (signed-byte 16) :big)



;;;;;;;;;;;;;;;;;;;;;
;;; integer types ;;;
;;;;;;;;;;;;;;;;;;;;;

(declaim (inline decode-uint decode-sint encode-int))

(defun decode-uint (buffer endian &optional (start 0) (bits 32))
  (declare (fixnum start bits)
           (type octet-vector buffer))
  (do* ((n (the fixnum (/ bits 8)))
        (end (+ start n))
        (accum 0)
        (i start (1+ i))
        (k (the fixnum 
             (ecase endian 
               (:little 0)
               (:big (* 8 (1- n)))))
           (the fixnum
             (ecase endian
               (:little (+ k 8))
               (:big (- k 8))))))
       ((= i end) (values accum n))
    (setq accum (dpb (aref buffer i)
                     (byte 8 k)
                     accum))
    ))
            
    ;; (let ((accum 0)
    ;;       (count (/ bits 8)))
    ;;   (declare (integer accum))
    ;;   (dotimes (i count)
    ;;     (declare (fixnum i))
    ;;     (setf (ldb (byte 8 (* 8 i)) accum)
    ;;           (aref-endian buffer i start count endian)))
    ;;   (values accum (/ bits 8))))


(defun decode-sint (buffer endian &optional (start 0) (bits 32) )
  (declare (fixnum start bits)
           (type (octet-vector) buffer))
  (let ((result (decode-uint buffer endian start bits))
        (count (/ bits 8)))
    (declare (fixnum count))
    (when (logbitp (1- bits) result)
      (decf result (ash 1 bits)))
    (values result count)))


(defun encode-int (val endian &optional buffer (start 0) (bits 32))
  (declare (integer val)
           (fixnum start bits)
           (symbol endian)
           (type (or null octet-vector) buffer))
  (do* ((n (the fixnum (/ bits 8)))
        (buffer (or buffer (make-octet-vector n)))
        (end (+ start n))
        (i start (1+ i))
        (k (the fixnum
             (ecase endian 
               (:little 0)
               (:big (* 8 (1- n)))))
           (the fixnum
             (ecase endian
               (:little (+ 8 k))
               (:big (- k 8))))))
       ((= i end) (values n buffer))
    (setf (aref buffer i)
          (ldb (byte 8 k) val))))

  ;; (let* ((count (/ bits 8))
  ;;        (buffer (or buffer (make-octet-vector count))))
  ;;   (declare (type octet-vector buffer))
  ;;   (dotimes (i count)
  ;;     (setf (aref-endian buffer i start count endian)
  ;;           (ldb (byte 8 (* i 8)) val)))
  ;;   (values (/ bits 8) buffer)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; floating point types ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro def-cffi-cast (name from-lisp-type from-c-type to-c-type)
  (let ((val (gensym))
        (x (gensym)))
    `(progn 
       (declaim (inline ,name))
       (defun ,name (,val)
         "Use CFFI to extract the bits of val by a C-like cast."
         (declare (type ,from-lisp-type ,val))
         (cffi:with-foreign-object (,x ,from-c-type)
           (setf (cffi:mem-ref ,x ,from-c-type) ,val)
           (cffi:mem-ref ,x ,to-c-type))))))
                        
(def-cffi-cast scary-single-float-bits single-float :float :uint32) 
(def-cffi-cast scary-make-single-float (unsigned-byte 32) :uint32 :float)

(def-cffi-cast scary-double-float-bits double-float :double :uint64) 
(def-cffi-cast scary-make-double-float (unsigned-byte 64) :uint64 :double)



(declaim (inline decode-double-float 
                 encode-double-float))
(defun decode-double-float (buffer endian &optional (start 0))
  (declare (type octet-vector buffer)
           (symbol endian))
  (scary-make-double-float (decode-uint buffer endian start 64)))

(defun encode-double-float (val endian &optional buffer (start 0))
  (let ((bits (scary-double-float-bits val))
        (buffer (or buffer (make-octet-vector 8))))
    (declare (type octet-vector buffer))
    (encode-int bits endian buffer start 64)))


(defun decode-single-float (buffer endian &optional (start 0))
  (declare (type octet-vector buffer))
  (scary-make-single-float (decode-sint buffer endian start)))

(defun encode-single-float (val endian &optional buffer (start 0))
  (let ((bits (scary-single-float-bits val))
        (buffer (or buffer (make-octet-vector 4))))
    (declare (type octet-vector buffer))
    (encode-int bits endian buffer start)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fixed integer types ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defmacro def-uint-decoder (name bits endian)
;;     `(progn
;;        (declaim (inline ,name))
;;        (defun ,name (buffer &optional (start 0))
;;          (declare (type octet-vector buffer)
;;                   (type fixnum start))
;;          (logior ,@(loop 
;;                       with n = (/ bits 8)
;;                       for i below n
;;                       for j = (ecase endian 
;;                                 (:little i)
;;                                 (:big (- n i 1)))
;;                       collect
;;                         `(ash (aref buffer (+ start ,i)) ,(* 8 j)))))))

;; (defmacro def-sint-decoder (name bits uint-decoder)
;;   `(progn
;;      (declaim (inline ,name))
;;      (defun ,name (buffer &optional (start 0))
;;        (declare (type octet-vector buffer)
;;                 (type fixnum start))
;;        (let ((u (,uint-decoder buffer start)))
;;          (declare (type (unsigned-byte ,bits) u))
;;          (let ((s (if (logbitp ,(1- bits) u)
;;                       (- u ,(ash 1 bits))
;;                       u)))
;;            (declare (type (signed-byte ,bits) s))
;;            s)))))

;; (def-uint-decoder decode-uint-32-le 32 :little)
;; (def-uint-decoder decode-uint-64-le 64 :little)
;; (def-uint-decoder decode-uint-32-be 32 :big)
;; (def-uint-decoder decode-uint-64-be 64 :big)

;; (def-sint-decoder decode-sint-32-le 32 decode-uint-32-le)
;; (def-sint-decoder decode-sint-64-le 64 decode-uint-64-le)
;; (def-sint-decoder decode-sint-32-be 32 decode-uint-32-be)
;; (def-sint-decoder decode-sint-64-be 64 decode-uint-64-be)


;;;;;;;;;;;;;;;;;;;;
;;; varint types ;;;
;;;;;;;;;;;;;;;;;;;;

;; arbitrary precision zig-zagging

(defun varint-zigzag (value)
  (declare (integer value))
  (- (* 2 (abs value))
     (* (signum value)
        (ash (1- (signum value)) -1))))

(defun varint-unzigzag (value)
  (declare (integer value))
  (let ((lowbit (ldb (byte 1 0) value)))
    (* (ash (+ value lowbit) -1) 
       (- 1 (* 2 lowbit)))))
         


;; i don't know how to do this to arbitrary precision for negative
;; numbers.  The google implemention gives uint32_t and uint64_t.
;; Let's be unsigned.

(declaim (inline uvarint-size))
(defun uvarint-size (value)
  (declare (type (integer 0) value))
  (max 1 (ceiling (integer-length value) 7)))

(declaim (inline uvarint-size))
(defun svarint-size (value)
  (uvarint-size (varint-zigzag value)))

(defun encode-uvarint (value &optional 
                       (buffer (make-octet-vector (uvarint-size value)))
                       (start 0))
  (declare (type (integer 0) value)
           (type octet-vector buffer)
           (type fixnum start))
  (loop 
     for v = value then (ash v -7)
     for v-next = (ash v -7)
     for j from 0 below most-positive-fixnum
     for i = (+ start j)
     until (or (and (zerop v) (> j 0))
               ;; cut out negative handling.
               ;(and (< value 0) 
                    ;;(= j 10) ;; i guess we'll use google's arbitrary limit...
                    ;;; fixup last element
                    ;(setf (ldb (byte 1 7) (aref buffer (1- i)))
                          ;0)))
               )
     do (progn 
          ;(format t "~&i: ~A, v: ~A, v-next: ~A" i v v-next)
          (setf (aref buffer i)
                (logior (ldb (byte 7 0) v)
                        (if (zerop v-next) 0 (ash 1 7)))))
     finally (return (values (- i start) buffer))))


(defun decode-uvarint (buffer start)
  (declare (type octet-vector buffer)
           (fixnum start))
  (loop
     for i from 1      ; octets read
     for j from start  ; position in buffer
     for k from 0 by 7 ; position in integer
     for octet = (aref buffer j)
     for piece = (ldb (byte 7 0) octet)
     for accum = piece then (dpb piece (byte 7 k) accum)
     when (not (logbitp 7 octet))
     return (values accum i)))

;; ;; Not really any faster
;; (defun decode-uvarint (buffer start)
;;   (declare (type octet-vector buffer)
;;            (fixnum start))
;;   (labels ((mask (x)
;;              (logand #x7F x))
;;            (extract (index shift)
;;              (ash (mask (aref buffer (+ start index)))
;;                   shift)))
;;     (cond ;; fast paths
;;       ((not (logbitp 7 (aref buffer start)))
;;        (values (aref buffer start) 
;;                1))
;;       ((not (logbitp 7 (aref buffer (+ start 1))))
;;        (values (logior (mask (aref buffer start)) 
;;                        (extract 1 7))
;;                2))
;;       ((not (logbitp 7 (aref buffer (+ start 2))))
;;        (values (logior (mask (aref buffer start))
;;                        (extract 1 7)
;;                        (extract 2 14))
;;                3))
;;       ((not (logbitp 7 (aref buffer (+ start 3))))
;;        (values (logior (mask (aref buffer start))
;;                        (extract 1 7)
;;                        (extract 2 14)
;;                        (extract 3 21))
;;                4))
;;       (t (decode-uvarint-slow buffer start)))))

(defun encode-svarint (value &optional
                       (buffer (make-octet-vector (svarint-size value)))
                       (start 0))
  (declare (type octet-vector buffer))
  (encode-uvarint (varint-zigzag value) buffer start))

(defun decode-svarint (buffer start)
  (declare (type octet-vector buffer))
  (multiple-value-bind (uv i)
      (decode-uvarint buffer start)
  (values (varint-unzigzag uv) i)))


;; ;; rather (quite) slow...
;; (defun read-octets (stream1 &key limit)
;;   "read up to limit bytes from stream or eof if limit is nil"
;;   (loop  with v =  (make-array 0 
;;                                :element-type '(unsigned-byte 8)
;;                                :adjustable t :fill-pointer t)
;;      for x = (read-byte stream1 nil nil)
;;      for i from 0
;;      until (or (null x) (and limit (>= i limit)))
;;      do (vector-push-extend x v)
;;      finally (return (values (make-array (length v) :element-type 'octet
;;                                   :initial-contents v)
;;                              i))))

(defun read-file-octets (filespec &key limit)
  (with-open-file (s filespec :element-type 'octet)
    (let ((buffer (make-octet-vector (if limit
                                         (max (file-length s) limit)
                                         (file-length s)))))
      (read-sequence buffer s)
      buffer)))

;;;;;;;;;;;;;;;
;;; strings ;;;
;;;;;;;;;;;;;;;

#-sbcl
(defun encode-utf8 (string 
                         &key 
                         (string-start 0) (string-end (length string))
                         buffer (buffer-start 0))
  (let ((buffer (or buffer (make-octet-vector (- string-end string-start)))))
    (loop 
       for i-b from buffer-start
       for i-s from string-start below string-end
       do (setf (aref buffer i-b)
                (char-code (aref string i-s))))
    (values (- string-end string-start)
            buffer)))
  
#-sbcl
(defun decode-utf8 (buffer &key
                         (buffer-start 0) (buffer-end (length buffer))
                         (string-start 0) 
                         (string (make-string (+ string-start 
                                                 (- buffer-end buffer-start)))))
  (loop 
     for i-s from string-start
     for i-b from buffer-start below buffer-end
     do (setf (aref string i-s)
              (code-char (aref buffer i-b))))
  (values string
          (- buffer-end buffer-start)))
      

#+sbcl
(defun encode-utf8 (string 
                    &key 
                    (string-start 0) (string-end (length string))
                    buffer (buffer-start 0))
  (let ((octets (sb-ext:string-to-octets string 
                                         :start string-start
                                         :end string-end)))
    (values (length octets)
            (if buffer
                (replace buffer octets :start1 buffer-start)
                octets))))

 
 
#+sbcl
(defun decode-utf8 (buffer &key
                    (string-start 0) string 
                    (buffer-start 0) (buffer-end (length buffer)))
  (let ((str (sb-ext:octets-to-string buffer 
                                      :start buffer-start 
                                      :end buffer-end
                                      :external-format :utf8)))
    (values (if string
               (replace string str :start1 string-start)
               str)
            (- buffer-end buffer-start))))

(defun utf8-size (string)
  (multiple-value-bind (size buffer) 
      (encode-utf8 string)
    (declare (ignore buffer))
    size))

(defun test ()
  (labels ((test-uint (value endian bits)
             (let ((buffer (make-octet-vector (/ bits 8))))
               (encode-int value endian buffer 0 bits)
               (= value (decode-uint buffer endian 0 bits))))
           (test-sint (value endian bits)
             (let ((buffer (make-octet-vector (/ bits 8))))
               (encode-int value endian buffer 0 bits)
               (= value (decode-sint buffer endian 0 bits))))
           (test-scary-single (x)
             (= x 
                (scary-make-single-float (scary-single-float-bits x))))
           (test-scary-double (x)
             (= x 
                (scary-make-double-float (scary-double-float-bits x))))
           (test-single (val buffer endian)
             (multiple-value-bind (i-enc buf-enc)
                 (encode-single-float val endian)
               (and (= i-enc 4)
                    (equalp buf-enc buffer)
                    (= val (decode-single-float buffer endian)))))
           (test-double (val buffer endian)
             (multiple-value-bind (i-enc buf-enc)
                 (encode-double-float val endian)
               (and (= i-enc 8)
                    (equalp buf-enc buffer)
                    (= val (decode-double-float buffer endian)))))
           (test-zigzag (original encoded)
             (and 
              (= (varint-zigzag original ) encoded)
              (= (varint-unzigzag encoded) original)))
           (test-uvarint-encoding (value expected-buffer)
             (multiple-value-bind (size buf)
                 (encode-uvarint value)
               (and (= size (length expected-buffer))
                    (equalp buf expected-buffer))))
           (test-uvarint (value)
             (multiple-value-bind (i-enc buf)
                 (encode-uvarint value)
               (multiple-value-bind (v-dec i-dec)
                   (decode-uvarint buf 0)
                 (and (= i-dec i-enc)
                      (= value v-dec)))))
           (test-svarint (value)
             (multiple-value-bind (i-enc buf)
                 (encode-svarint value)
               (multiple-value-bind (v-dec i-dec)
                   (decode-svarint buf 0)
                 (and (= i-dec i-enc)
                      (= value v-dec)))))
           (test-utf8 (string expected-bytes)
             (multiple-value-bind (size bytes) 
                 (encode-utf8 string)
               (and (= size (length expected-bytes))
                    (equalp bytes expected-bytes)
                    (multiple-value-bind (decoded-val decoded-size)
                        (decode-utf8 bytes)
                      (and (string= string decoded-val)
                           (= decoded-size (length expected-bytes)))))))
           )
               
    ;; test integer encoding
    (assert (test-uint 10 :little 32))
    (assert (test-uint 1024 :little 32))
    (assert (test-sint -10 :little 32))
    (assert (test-sint -10 :little 64))
    (assert (test-sint -4097 :little 64))
    (assert (test-sint 10 :little 64))
    (assert (test-uint 10 :big 32))
    (assert (test-uint 1024 :big 32))
    (assert (test-sint -10 :big 32))
    (assert (test-sint -10 :big 64))
    (assert (test-sint -4097 :big 64))
    (assert (test-sint 10 :big 64))


    ;; float encoding
    (assert (test-scary-single 1f0))
    (assert (test-scary-single (coerce pi 'single-float)))
    (assert (test-scary-double 1d0))
    (assert (test-scary-double (coerce pi 'double-float)))
    (assert (test-single 1f0  (octet-vector 0 0 128 63) :little))
    (assert (test-double 1d0  (octet-vector 0 0 0 0 0 0 240 63)  :little))


    ;; test varint zigzags based on google's examples
    (assert (test-zigzag 0  0))
    (assert (test-zigzag -1  1))
    (assert (test-zigzag 1  2)) 
   (assert (test-zigzag 2147483647  4294967294))
    (assert (test-zigzag -2147483648 4294967295))
    ;; varints
    ;; example encodings from the google docs
    (assert (test-uvarint-encoding 
             150 (octet-vector #x96 1)))
    (assert (test-uvarint-encoding 
             300 (octet-vector #b10101100 #b00000010)))
    (assert (test-uvarint 10))
    (assert (test-uvarint 100))
    (assert (test-uvarint 100000))
    (assert (equalp (multiple-value-bind (a b)
                        (encode-uvarint 0)
                      (declare (ignore a))
                      b)
                    (octet-vector 0)))
    ;; signed varints
    (assert (test-svarint 10))
    (assert (test-svarint 100))
    (assert (test-svarint 100000))
    (assert (test-svarint -10))
    (assert (test-svarint 100))
    (assert (test-svarint -100000))
    ;; utf8
    (assert (test-utf8 "testing" 
                       (octet-vector #x74 #x65 #x73 
                                     #x74 #x69 #x6e #x67)))
    )
  t)
