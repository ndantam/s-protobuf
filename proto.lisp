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

(defpackage :protocol-buffer
  (:nicknames :pb)
  (:use :cl :binio)
  (:export
   :pack
   :unpack
   :protocol-buffer))

(in-package :protocol-buffer)

;; generic function acessors
(defgeneric pack (protobuf &optional buf start))
(defgeneric packed-size (protobuf))

(defgeneric unpack (buffer protobuf &optional start))


(defparameter +types+ '(:double :float
                        :int32 :int64
                        :uint32 :uint64
                        :sint32 :sint64
                        :fixed32 :fixed64
                        :sfixed32 :sfixed64
                        :bool :string :bytes))

;; types
(deftype start-code () '(integer 0 5))



(defun fixed64-p (type)
  (case type
    ((:double :fixed64 :sfixed64) t)
    (otherwise nil)))

(defun fixed32-p (type)
  (case type
    ((:float :sfixed :fixed32) t)
    (otherwise nil)))


(defun fixed-p (type)
  (or (fixed32-p type)
      (fixed64-p type)))

(defun varint-p (type)
  (case type
    ((:bool :int32 :sint32 :uint32 :int64 :sint64 :enum :int32 :uint64)
     t)
    (otherwise nil)))

(defun svarint-p (type)
  (case type
    ((:sint32 :sint64) t)
    (otherwise nil)))

(defun uvarint-p (type)
  (and (varint-p type) (not (svarint-p type))))


(defun length-delim-p (type)
  (and (not (fixed64-p type))
       (not (fixed32-p type))
       (not (varint-p type))))

(defun fixed-size (type)
  (cond ((fixed32-p type) 4)
        ((fixed64-p type) 8)
        (t (error "Not a fixed type: ~A" type))))

(defun wire-typecode (type)
  (cond 
    ((varint-p type) 0)
    ((fixed64-p type) 1)
    ((fixed32-p type) 5)
    ((length-delim-p type) 2)
    (t 2)))

;; runtime support functions

(defun make-start-code (slot-position typecode)
  (logior (ash slot-position 3) typecode))


(defun encode-start-code (slot-position typecode buffer start)
  (encode-uvarint (make-start-code slot-position typecode)
                 buffer start))


(defun proto-test ()
  (labels ((test-start-code (type pos result)
             (let ((buffer (make-octet-vector 1)))
               (encode-start-code pos (wire-typecode type) buffer 0)
               (equalp buffer (octet-vector result)))))
    ;; from the google docs
    (assert (test-start-code :int32 1 #16r08))
    (assert (test-start-code :string 2 #16r12))
    t))

(defun packed-uvarint-size (array)
  (loop for x across array 
     summing (binio::uvarint-size x)))

(defun packed-svarint-size (array)
  (loop for x across array 
     summing (binio::svarint-size x)))

(defun length-delim-size (length)
  (+ (binio::uvarint-size length) length))

;(defun packed-type-size (array type)
  ;)
