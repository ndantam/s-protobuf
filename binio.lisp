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
   :decode-uint :decode-sint 
   :encode-int 
   :encode-double-float :decode-double-float
   :encode-svarint :decode-svarint
   :encode-uvarint :decode-uvarint
   :make-octet-vector :octet-vector
   :encode-utf8 :decode-utf8
   ))

;; types u?int{8,16,32,63}, double, float


(in-package :binio)

;; encoding fuctions:
;;  (value &optional buffer start) => (values bytes-encoded buffer)
;; decoding fuctions:
;;  (buffer &optional start) => (values value bytes-decoded)


;; types
(deftype octet () '(unsigned-byte 8))
(deftype octet-vector (&optional (count '*)) `(simple-array octet ,count))

(defun make-octet-vector (count)
  (make-array count :element-type 'octet))

(defun octet-vector (&rest args)
  (let ((v (make-octet-vector (length args))))
    (loop 
       for x in args
       for i from 0
         do
         (setf (aref v i) x))
    v))

;; some endian handling

(defun index-endian (index start count endian)
  (declare (fixnum start count index))
  (case endian
    (:little 
     (+ start index)) 
    (:big 
     (+ start count -1 (- index)))
    (otherwise 
     (error "endian must be :big or :little, not ~S" endian))))

(defun aref-endian (buffer index start count endian)
  (declare (fixnum start count index))
  (aref buffer (index-endian index start count endian)))

(defun (setf aref-endian) (value buffer index start count endian)
  (declare (fixnum start count index))
  (setf (aref buffer (index-endian index start count endian))
        value))

;; integer types

(defun decode-uint (buffer endian &optional (start 0) (bits 32))
  (declare (fixnum start bits)
           (type (octet-vector) buffer))
  (let ((accum 0)
        (count (/ bits 8)))
    (declare (integer accum))
    (dotimes (i count)
      (setf (ldb (byte 8 (* 8 i)) accum)
            (aref-endian buffer i start count endian)))
    accum))


(defun decode-sint (buffer endian &optional (start 0) (bits 32) )
  (declare (fixnum start bits)
           (type (octet-vector) buffer))
  (let ((result (decode-uint buffer endian start bits))
        (count (/ bits 8)))
    (when (= (ldb (byte 1 (1- (* 8 count))) result) 1) ; sign bit, negative
      (decf result (ash 1 (* 8 count))))
    result))

(defun encode-int (val endian &optional buffer (start 0) (bits 32))
  (declare (integer val)
           (fixnum start bits)
           (symbol endian))
  (let* ((count (/ bits 8))
         (buffer (or buffer (make-octet-vector count))))
    (dotimes (i count)
      (setf (aref-endian buffer i start count endian)
            (ldb (byte 8 (* i 8)) val)))
    buffer))


;; floating point types

(defun decode-double-float (buffer endian &optional (start 0))
  (let ((ilow  (decode-uint buffer endian start))
        (ihi (decode-sint buffer endian (+ 4 start))))
    (case endian
      (:little (sb-kernel:make-double-float ihi ilow))
      (:big (sb-kernel:make-double-float ilow ihi)))))

(defun encode-double-float (val endian &optional buffer (start 0))
  (let ((high (sb-kernel:double-float-high-bits val))
        (low (sb-kernel:double-float-low-bits val))
        (buffer (or buffer (make-octet-vector 8))))
    (let ((i (dpb high (byte 32 32) low)))
      (encode-int i endian buffer start 64))
    buffer))


 
(defun decode-single-float (buffer endian &optional (start 0))
  (let ((i (decode-sint buffer endian start)))
    (sb-kernel:make-single-float i)))

(defun encode-single-float (val endian &optional buffer (start 0))
  (let ((bits (sb-kernel:single-float-bits val))
        (buffer (or buffer (make-octet-vector 4))))
    (encode-int bits endian buffer start)))




;; varint types

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

(defun uvarint-size (value)
  (declare (type (integer 0) value))
  (loop 
     for v = value then (ash v -7)
     for i from 0
     until (zerop v)
     finally (return i)))

(defun svarint-size (value)
  (uvarint-size (varint-zigzag value)))

(defun encode-uvarint (value &optional 
                       (buffer (make-octet-vector (uvarint-size value)))
                       (start 0))
  (declare (type (integer 0) value))
  (loop 
     for v = value then (ash v -7)
     for v-next = (ash v -7)
     for j from 0
     for i = (+ start j)
     until (or (zerop v) 
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
  (loop
     with accum = 0
     for i from 0
     for j = (+ i start)
     do (setf (ldb (byte 7 (* i 7)) accum)
              (ldb (byte 7 0) (aref buffer j)))
     until (zerop (ldb (byte 1 7) (aref buffer j)))
     finally (return accum)))

(defun encode-svarint (value &optional
                       (buffer (make-octet-vector (svarint-size value)))
                       (start 0))
  (values (encode-uvarint (varint-zigzag value) buffer start) buffer))

(defun decode-svarint (buffer start)
  (varint-unzigzag (decode-uvarint buffer start)))

;; strings

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

                                         
  
(defun decode-utf8 (buffer &key
                    (string-start 0) string 
                    (buffer-start 0) (buffer-end (length buffer)))
  (let ((str (sb-ext:octets-to-string buffer 
                                      :start buffer-start 
                                      :end buffer-end
                                      :external-format :utf8)))
    (if string
        (replace string str :start1 string-start)
        str)))


(defun utf8-size (string)
  (multiple-value-bind (size buffer) 
      (encode-utf8 string)
    (declare (ignore buffer))
    size))

(defun binio-test ()
  (labels ((test-uint (value endian bits)
             (let ((buffer (make-octet-vector (/ bits 8))))
               (encode-int value endian buffer 0 bits)
               (= value (decode-uint buffer endian 0 bits))))
           (test-sint (value endian bits)
             (let ((buffer (make-octet-vector (/ bits 8))))
               (encode-int value endian buffer 0 bits)
               (= value (decode-sint buffer endian 0 bits))))

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
             (let ((buffer (make-octet-vector 12)))
               (encode-uvarint value buffer 0)
               (= value (decode-uvarint buffer 0))))
           (test-svarint (value)
             (let ((buffer (make-octet-vector 12)))
               (encode-svarint value buffer 0)
               (= value (decode-svarint buffer 0))))

           (test-utf8 (string expected-bytes)
             (multiple-value-bind (size bytes) 
                 (encode-utf8 string)
               (and (= size (length expected-bytes))
                    (equalp bytes expected-bytes))))
           )
               
    ;; test integer encoding
    (assert (test-uint 10 :little 32))
    (assert (test-uint 1024 :little 32))
    (assert (test-sint -10 :little 32))
    (assert (test-sint -10 :little 64))
    (assert (test-sint -4097 :little 64))
    ;; test varint zigzags based on google's examples
    (assert (test-zigzag 0  0))
    (assert (test-zigzag -1  1))
    (assert (test-zigzag 1  2))
    (assert (test-zigzag 2147483647  4294967294))
    (assert (test-zigzag -2147483648 4294967295))
    ;; varints
    ;; example encodings from the google docs
    (assert (test-uvarint-encoding 
             150 (octet-vector #16r96 1)))
    (assert (test-uvarint-encoding 
             300 (octet-vector #2r10101100 #2r00000010)))
    (assert (test-uvarint 10))
    (assert (test-uvarint 100))
    (assert (test-uvarint 100000))
    ;; signed varints
    (assert (test-svarint 10))
    (assert (test-svarint 100))
    (assert (test-svarint 100000))
    (assert (test-svarint -10))
    (assert (test-svarint 100))
    (assert (test-svarint -100000))
    ;; utf8
    (assert (test-utf8 "testing" 
                       (octet-vector #16r74 #16r65 #16r73 
                                     #16r74 #16r69 #16r6e #16r67)))
    )
  t)
