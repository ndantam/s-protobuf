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
                               

(defun unmangle (str)
  (with-output-to-string (s)
    (loop for i from 0 below (length str)
         do
         (cond 
           ((eq (aref str i) #\_)
            (write-char #\- s))
           (t (write-char (char-upcase (aref str i)) s))))))

(defun unmangle-slot (object slot)
  (unmangle (slot-value object slot)))


(defun sanitize-package (file-proto)
  (if (slot-value file-proto 'package)
      (find-package (intern (unmangle-slot file-proto 'package)
                            :keyword))
      (find-package :cl-user)))

(defun sanitize-name (name package)
  (intern (unmangle name) package))

(defun sanitize-name-slot (object slot package)
  (sanitize-name (slot-value object slot) package))

(defgeneric sanitize (object &key parent package)
  (:documentation 
  "Convert the structure based protocol buffer representation
  obtainened by parsing the binary output of protoc into
  S-Expressions"))

(defun sanitizer (parent package)
  (lambda (object) (sanitize object :parent parent :package package)))

(defmethod sanitize ((object t) &key parent package)
  (declare (ignore parent package))
  object)

;; multi-file container
(defmethod sanitize ((object file-descriptor-set) &key parent package)
  (declare (ignore parent package))
  (apply #'nconc (map 'list #'sanitize (slot-value object 'file))))

;; single-file container 
(defmethod sanitize ((object file-descriptor-proto) &key parent package)
  (declare (ignore parent package))
  (nconc 
   (map 'list (sanitizer object (sanitize-package object))
        (slot-value object 'enum-type))
   (map 'list (sanitizer object (sanitize-package object) )
        (slot-value object 'message-type))))


;; a message
(defmethod sanitize ((object descriptor-proto) &key parent package)
  (declare (ignore parent))
  `(message ,(sanitize-name-slot object 'name package)
            ,@(map 'list (sanitizer object package) (slot-value object 'field))))

;; a message field
(defmethod sanitize ((object field-descriptor-proto) &key parent package)
  (declare (ignore parent))
  `(field ,(sanitize-name-slot object 'name package)
          ,(slot-value object 'type)
          ,(slot-value object 'number)
          ,@(if (eq (slot-value object 'label) :label-optional)
                '(:optional t))
          ,@(if (eq (slot-value object 'label) :label-repeated)
                '(:repeated t))
          ,@(if (eq (slot-value object 'label) :label-required)
                '(:required t))))

;; enum
(defmethod sanitize ((object enum-descriptor-proto) &key parent package)
  (declare (ignore parent))
  `(enum ,(sanitize-name-slot object 'name package) 
         ,@(map 'list (sanitizer object package) 
                (slot-value object 'value))))
;; enum value
(defmethod sanitize ((object enum-value-descriptor-proto) &key parent package)
  (declare (ignore parent package))
  (list (sanitize-name-slot object 'name (find-package :keyword))
        (slot-value object 'number)))



(let ((set (pb:unpack (binio::read-file-octets 
                       "/home/ntd/src/s-protobuf/tests/test.protobin")
           (make-instance 'file-descriptor-set))))
  ;(princ set)
  (prin1 (sanitize set))
  nil)
  
