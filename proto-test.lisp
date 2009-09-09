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


;; Protocol Buffer Compiler for CL
;; Author: Neil T. Dantam



(defpackage :protocol-buffer-test
  (:nicknames :proto-test)
  (:use :cl))


(in-package :proto-test)


;; The examples from :
;; http://code.google.com/apis/protocolbuffers/docs/encoding.html

(defparameter *test-form-1* 
  '(message test1
    (field a :int32 1)))

(defparameter *test-form-2* 
  '(message test2
    (field b :string 2)))

(defparameter *test-form-3* 
  '(message test3
    (field c test1 3)))

(defparameter *test-form-4* 
  '(message test4
    (field d :int32 4 :repeated t :packed t)))

;(macroexpand-1
;  '(protoc::compile-proto test2))

;(protoc::def-packed-size *test-form-1* *package*)
;(protoc::msg-defpack *test-form-1* *package*)

(protoc::msg-defpack *test-form-2* *package*)

(defun oct->hex (array)
  (map 'vector (lambda (elt)
                 (format nil "~x" elt))
       array))

(defun test-encoding (protobuf expected-count expected-buffer)
  (multiple-value-bind (count buffer) (pb::pack protobuf)
    (let ((count= (= count expected-count))
          (buf=  (equalp buffer expected-buffer)))
      (and count= buf=))))


(protoc::eval-proto *test-form-1* (find-package :proto-test))
(protoc::eval-proto *test-form-2* (find-package :proto-test))

(defun test-1 (test-buffer type slot slot-value &key (test #'equalp))
  (let ((enc-inst (make-instance type))
        (dec-inst (make-instance type)))
    (setf (slot-value enc-inst slot) slot-value)
    ;; test encoding
    (multiple-value-bind (length buffer)
        (pb:pack enc-inst)
      (assert (= length (length test-buffer) (length buffer)))
      (assert (equalp buffer test-buffer)))
    ;; test decoding
    (multiple-value-bind (dec-inst* length)
        (pb:unpack test-buffer dec-inst)
      (assert (= length (length test-buffer)))
      (assert (eq dec-inst* dec-inst))
      (assert (funcall test slot-value (slot-value dec-inst slot))))))

(defun test ()
  (format t "package ~A~%" *package*)
  ;; test binio
  (assert (binio::test))



  ;; string decoding
  (assert (string= "testing"
                   (pb::decode-string (binio:octet-vector 
                                       #16r12 #16r07
                                       #16r74 #16r65 #16r73 
                                       #16r74 #16r69 #16r6e #16r67)
                                      1)))

  (let ((buffer-1 (binio:octet-vector #16r8 #16r96 #16r1))
        (buffer-2 (binio:octet-vector 
                   #16r12 #16r07
                   #16r74 #16r65 #16r73 
                   #16r74 #16r69 #16r6e #16r67))
        (buffer-3 (binio:octet-vector 
                   #16r1a #16r03
                   #16r08 #16r96 #16r01))
        (buffer-4 (binio:octet-vector #16r22 #16r06
                                      #16r03 
                                      #16r8e #16r02
                                      #16r9e #16ra7 #16r5)))
  ;; Packing Tests
  ;; http://code.google.com/apis/protocolbuffers/docs/encoding.html

  (protoc::eval-proto *test-form-1* (find-package :proto-test))
  (protoc::eval-proto *test-form-2* (find-package :proto-test))

  ;; test 1
  (test-1 buffer-1 'test1 'a 150)
  ;; test 2
  (test-1 buffer-2 'test2 'b "testing")
  ;; test 3
  (let ((value (make-instance 'test1)))
    (setf (slot-value value 'a) 150)
    (test-1 buffer-3 'test3 'c value 
            :test (lambda (a b)
                    (= (slot-value a 'a)
                       (slot-value b 'a)))))

  ;(protoc::eval-proto *test-form-4* (find-package :proto-test))
  ;; Test 4, repeated packed
  (let ((x (make-instance 'test4 )))
    (setf (slot-value x 'proto-test::d) (vector 3 270 86942))
    (assert (= 8 (pb::packed-size x)))
    (assert (test-encoding x 
                           8 
                           buffer-4))
                                               
    t)

  t))

