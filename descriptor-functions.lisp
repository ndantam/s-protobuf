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

;;(in-package :proto-desc)
(in-package :protoc)

(defun format-vector (stream vector)
  (loop for x across vector
     do (format stream "~&~A" x)))


(defun slot-bound-value (object slot &optional default-value)
  (or (pb::slot-bound-setp object slot) default-value))

;; FIXME: these print-object definitions are incomplete

(defmethod print-object ((object file-descriptor-set) stream)
  (format stream "~&// File Descriptor Set:")
  (format-vector stream  (slot-value object 'file)))


(defmethod print-object ((object file-descriptor-proto) stream)
  (format stream "~&// BEGIN File Descriptor Proto: ")
  (when (slot-boundp  object 'name)
    (format stream "~&//     Name: ~A" (slot-value object 'name)))
  (when (slot-boundp  object 'package)
    (format stream "~&//     Package: ~A" (slot-value object 'package)))
  (when (slot-boundp object 'message-type)
    (format stream "~&//   Messages:")
    (format-vector stream (slot-value object 'message-type)))
  (format stream "~&// END File Descriptor Proto: ")
)



(defmethod print-object ((object descriptor-proto) stream)
  (format stream "~&message ~A {"
          (slot-bound-value object 'name))
  (format-vector stream (slot-bound-value object 'field))
  (format stream "~&}"))


(defmethod print-object ((object field-descriptor-proto) stream)
  (format stream "    ~A ~A ~A = ~A"
          (or (slot-bound-value object 'type-name)
              (string-downcase (subseq (symbol-name (slot-bound-value object 'label))
                                       6)))
          (string-downcase (subseq (symbol-name (slot-bound-value object 'type))
                                   0))
          (slot-value object 'name)
          (slot-value object 'number))
  (when (pb::slot-bound-setp object 'options)
    (prin1 (slot-value object 'options) stream))
  (write-string ";" stream))
                               

(defmethod print-object ((object field-options) stream)
  (when (pb::slot-bound-setp object 'packed)
    (format stream "[packed=~A]" (slot-value object 'packed))))

(defun unmangle (str)
  (with-output-to-string (s)
    (loop for i from 0 below (length str)
         do
         (cond 
           ((or (eq (aref str i) #\_)
                (eq (aref str i) #\.))
            (write-char #\- s))
           (t (write-char (char-upcase (aref str i)) s))))))

(defun unmangle-slot (object slot)
  (unmangle (slot-value object slot)))

(defun lookup-package-name (name)
  (or (find-package (intern name :keyword))
      :cl-user))

(defun sanitize-package (file-proto)
  (if (slot-value file-proto 'package)
      (lookup-package-name  (unmangle-slot file-proto 'package))
      (find-package :cl-user)))

(defun sanitize-name (name package)
  (let ((name name) (package package))
    (when (eq (aref name 0) #\.)
      (setq package 
            (lookup-package-name 
             (unmangle (subseq name 1 (position #\. name :start 1)))))
      (setq name (subseq name (1+ (position #\. name :start 1)))))
    (intern (unmangle name) package)))

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
  (let ((sanitizer (sanitizer object (sanitize-package object))))
    (nconc 
     (map 'list sanitizer
          (slot-bound-value object 'enum-type))
     (apply #'nconc 
            (map 'list sanitizer 
                 (slot-value object 'message-type))))))


;; a message
(defmethod sanitize ((object descriptor-proto) &key parent package)
  (declare (ignore parent))
  `((message ,(sanitize-name-slot object 'name package)
             ,@(map 'list (sanitizer nil package) 
                    (slot-bound-value object 'enum-type))
             ,@(map 'list (sanitizer object package) 
                    (slot-value object 'field)))))

(defun sanitize-type (field-desc package)
  (let ((base-type (slot-value field-desc 'type)))
    (case base-type
      ((:enum :message) (sanitize-name-slot field-desc 'type-name package))
      (otherwise base-type))))

;; a message field
(defmethod sanitize ((object field-descriptor-proto) &key parent package)
  (declare (ignore parent))
  `(field ,(sanitize-name-slot object 'name package)
          ,(sanitize-type object package)
          ,(slot-value object 'number)
          ,@(if (eq (slot-value object 'label) :label-optional)
                '(:optional t))
          ,@(if (eq (slot-value object 'label) :label-repeated)
                '(:repeated t))
          ,@(if (eq (slot-value object 'label) :label-required)
                '(:required t))
          ,@(if (and (pb::slot-bound-setp object 'options)
                     (pb::slot-bound-setp (slot-value object 'options)
                                      'packed))
                (progn
                  (assert  (eq (slot-value object 'label) :label-repeated) ()
                           "Can't have packed, nonrepeated field")
                  '(:packed t)))))

;; enum
(defmethod sanitize ((object enum-descriptor-proto) &key parent package)
  (let ((name (if (eq (type-of parent) 'descriptor-proto)
                  (concatenate 'string
                               (slot-value parent 'name) "-"
                               (slot-value object 'name))
                  (slot-value object 'name))))
      ;(format t "~&~S~&"  (type-of parent))
      ;(format t "~&No~&"))
  `(enum ,(sanitize-name name package)
         ,@(map 'list (sanitizer object package) 
                (slot-value object 'value)))))
;; enum value
(defmethod sanitize ((object enum-value-descriptor-proto) &key parent package)
  (declare (ignore parent package))
  (list (sanitize-name-slot object 'name (find-package :keyword))
        (slot-value object 'number)))


(defun load-proto-structs (filespec)
  (pb:unpack 
   (binio::read-file-octets filespec)
   (make-instance 'file-descriptor-set)))

(defun sanitize-file (filespec)
  (let ((set (load-proto-structs filespec)))
    (sanitize set)))


(defun macroize-se (se)
  (mapcan (lambda (a)
            (cond 
              ((protoc::symbol-string= (car a) 'message)
               (protoc::gen-msg-defs (cadr a) (cddr a)))
               ;(list (cons 'protoc::def-proto-msg (cdr a))))
              ((protoc::symbol-string= (car a) 'enum)
               (setf (get (cadr a) 'enum) t) ;; must set here
               (list (cons 'protoc::def-proto-enum (cdr a))))))
          se))
              

(defmacro load-proto-set (filespec)
  (cons 'progn 
        (macroize-se (sanitize-file filespec))))



(defun get-proto-set (filespec)
  (cons 'progn 
        (macroize-se (sanitize-file filespec))))

(defmacro dump-proto-set (filespec)
  (cons 'progn 
        (macroize-se (sanitize-file filespec))))

