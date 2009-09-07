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

(defun test ()
  (format t "package ~A~%" *package*)
  ;; test binio
  (assert (binio::test))

  ;; Packing Tests

  ;; Test1 from 
  ;; http://code.google.com/apis/protocolbuffers/docs/encoding.html
  (protoc::eval-proto *test-form-1* (find-package :proto-test))
  (let ((x (make-instance 'test1 )))
    (setf (slot-value x 'a) 150)
    (slot-value x 'a)
    (assert (= 3 (pb::packed-size x)))
    (assert (test-encoding x 3 (binio:octet-vector #16r8 #16r96 #16r1)))
    t)
  ;; Test2 from 
  ;; http://code.google.com/apis/protocolbuffers/docs/encoding.html
  (protoc::eval-proto *test-form-2* (find-package :proto-test))
  (let ((x (make-instance 'test2))
        );(size 9))
    (setf (slot-value x 'b) "testing")
    (assert (= 9 (pb::packed-size x)))
    (assert (test-encoding x 9 
                           (binio:octet-vector 
                            #16r12 #16r07
                            #16r74 #16r65 #16r73 
                            #16r74 #16r69 #16r6e #16r67)))
                                        ;(pb::pack x)

    t)
  ;; Test 3
  (let ((msg1 (make-instance 'proto-test::test1))
        (msg3 (make-instance 'proto-test::test3))
        )
    (setf (slot-value msg1 'proto-test::a) 150)
    (setf (slot-value msg3 'proto-test::c) msg1)
    (assert (test-encoding msg3 5 
                           (binio:octet-vector 
                            #16r1a #16r03
                            #16r08 #16r96 #16r01))))

  ;; Test 4, repeated packed
  (let ((x (make-instance 'test4 )))
    (setf (slot-value x 'proto-test::d) (vector 3 270 86942))
    (assert (= 8 (pb::packed-size x)))
    (assert (test-encoding x 
                           8 
                           (binio:octet-vector #16r22 #16r06
                                               #16r03 
                                               #16r8e #16r02
                                               #16r9e #16ra7 #16r5)))
                                               
    t)
  ;; string decoding
  (assert (string= "testing"
                   (pb::decode-string (binio:octet-vector 
                                       #16r12 #16r07
                                       #16r74 #16r65 #16r73 
                                       #16r74 #16r69 #16r6e #16r67)
                                      1)))
  ;; unpack test 1
  (let ((msg1 (make-instance 'test1)))
    (pb::unpack (binio::octet-vector 8 150 1) msg1)
    (assert (= 150 (slot-value msg1 'proto-test::a))))

  ;; unpack test 2
  (let ((msg2 (make-instance 'test2)))
    (pb::unpack (binio:octet-vector 
                 #16r12
                 #16r07
                 #16r74 #16r65 #16r73 
                 #16r74 #16r69 #16r6e #16r67)
                msg2)
    (assert (string= "testing" 
                     (slot-value msg2 'b))))
  t)

