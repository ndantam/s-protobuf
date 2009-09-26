;;; -*- Lisp -*-
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

;; Author: Neil T. Dantam

(in-package :proto-desc)

(defun format-vector (stream vector)
  (loop for x across vector
     do (format stream "~&~A" x)))


;; FIXME: these print-object definitions are incomplete

(defmethod print-object ((object file-descriptor-set) stream)
  (format stream "~&// File Descriptor Set:")
  (format-vector stream  (slot-value object 'file)))


(defmethod print-object ((object file-descriptor-proto) stream)
  (format stream "~&// BEGIN File Descriptor Proto: ")
  (format stream "~&//     Name: ~A" (slot-value object 'name))
  (format stream "~&//     Package: ~A" (slot-value object 'package))
  (format stream "~&//   Messages:")
  (format-vector stream (slot-value object 'message-type))
  (format stream "~&// END File Descriptor Proto: ")
)


(defmethod print-object ((object descriptor-proto) stream)
  (format stream "~&message ~A {"
          (slot-value object 'name))
  (format-vector stream (slot-value object 'field))
  (format stream "~&}"))


(defmethod print-object ((object field-descriptor-proto) stream)
  (format stream "    ~A ~A ~A = ~A;"
          (or (slot-value object 'type-name)
              (string-downcase (subseq (symbol-name (slot-value object 'label))
                                       6)))
          (string-downcase (subseq (symbol-name (slot-value object 'type))
                                   0))
          (slot-value object 'name)
          (slot-value object 'number)))
                               


(defgeneric sanitize (object &optional parent)
  (:documentation 
  "Convert the structure based protocol buffer representation
  obtainened by parsing the binary output of protoc into
  S-Expressions"))

(defun sanitizer (&optional parent)
  (lambda (object) (sanitize object parent)))

(defmethod sanitize ((object t) &optional parent)
  (declare (ignore parent))
  object)

(defmethod sanitize ((object file-descriptor-set) &optional parent)
  (declare (ignore parent))
  (apply #'nconc (map 'list #'sanitize (slot-value object 'file))))

(defmethod sanitize ((object file-descriptor-proto) &optional parent)
  (declare (ignore parent))
  (map 'list #'sanitize (slot-value object 'message-type)))

(defmethod sanitize ((object descriptor-proto) &optional parent)
  (declare (ignore parent))
  `(message ,(slot-value object 'name)
            ,@(map 'list (sanitizer object) (slot-value object 'field))))

(defmethod sanitize ((object field-descriptor-proto) &optional parent)
  (declare (ignore parent))
  `(field ,(slot-value object 'name) 
          ,(slot-value object 'type)
          ,(slot-value object 'number)
          ,@(if (eq (slot-value object 'label) :label-optional)
                '(:optional t))
          ,@(if (eq (slot-value object 'label) :label-repeated)
                '(:repeated t))
          ,@(if (eq (slot-value object 'label) :label-required)
                '(:required t))))




(let ((set (pb:unpack (binio::read-file-octets "tests/test.protobin")
           (make-instance 'file-descriptor-set))))
  ;(princ set)
  (prin1 (sanitize set))
  nil)
  
