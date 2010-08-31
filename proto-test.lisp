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
  (:use :cl :protoc))


(in-package :proto-test)


;; The examples from :
;; http://code.google.com/apis/protocolbuffers/docs/encoding.html


;; (protoc::def-proto-msg test1
;;   (field a :int32 1))

;; (protoc::def-proto-msg test2
;;   (field b :string 2))

;; (protoc::def-proto-msg test3
;;   (field c test1 3))

;; (protoc::def-proto-msg test4
;;   (field d :int32 4 :repeated t :packed nil))

;; (protoc::def-proto-msg testx1
;;   (field a :sfixed32 1 :repeated t :packed nil))


;; (protoc::def-proto-msg testx2
;;   (enum e (:a 0) (:b 10) (:c 20))
;;   (field a testx2-e 1 :repeated nil :packed nil))

(load-proto-set "./tests/test.protobin")

;; (defun redef () 
;;   (protoc::def-proto-msg test1
;;     (field a :int32 1))
  
;;   (protoc::def-proto-msg test2
;;     (field b :string 2))

;;   (protoc::def-proto-msg test3
;;     (field c test1 3))
  
;;   (protoc::def-proto-msg test4
;;     (field d :int32 4 :repeated t :packed nil))
  
;;   (protoc::def-proto-msg testx1
;;     (field a :sfixed32 1 :repeated t :packed nil))

;;   (protoc::def-proto-msg testx2
;;     (enum e (:a 0) (:b 10) (:c 20))
;;     (field a testx2-e 1 :repeated nil :packed nil)) 
;; )


(defun oct->hex (array)
  (map 'vector (lambda (elt)
                 (format nil "~x" elt))
       array))

(defun test-encoding (protobuf expected-buffer)
  (multiple-value-bind (count buffer) (pb::pack protobuf)
    (let ((count= (= count (length expected-buffer)))
          (buf=  (equalp buffer expected-buffer)))
      (assert count=
              () "Wrong encoded length")
      (assert buf=
              () "Bad encoding: ~A" buffer )
      (and count= buf=))))



(defun test-1 (test-buffer type slot slot-value &key (test #'equalp))
  (format t "~&Testing type ~A for ~A = ~S~%" type slot slot-value)
  (let ((enc-inst (make-instance type))
        (dec-inst (make-instance type)))
    (setf (slot-value enc-inst slot) slot-value)
    ;; test encoding
    (test-encoding enc-inst test-buffer)
    ;; test decoding
    (multiple-value-bind (dec-inst* length)
        (pb:unpack test-buffer dec-inst)
      (assert (= length (length test-buffer)) () "Wrong decoded length")
      (assert (eq dec-inst* dec-inst) () "Instance mismatch")
      (assert (funcall test slot-value (slot-value dec-inst slot))
              () "Bad decoding"))))

(defun test ()
  ;(format t "package ~A~%" *package*)
  ;; test binio
  (assert (binio::test))



  ;; string decoding
  (assert (string= "testing"
                   (pb::decode-string (binio:octet-vector 
                                       #x12 #x07
                                       #x74 #x65 #x73 
                                       #x74 #x69 #x6e #x67)
                                      1)))

  (let ((buffer-1 (binio:octet-vector #x8 #x96 #x1))
        (buffer-2 (binio:octet-vector 
                   #x12 #x07
                   #x74 #x65 #x73 
                   #x74 #x69 #x6e #x67))
        (buffer-3 (binio:octet-vector 
                   #x1a #x03
                   #x08 #x96 #x01))
        ;; typecode is #x22 for packed
        (buffer-4 (binio:octet-vector #x20 ;#x06
                                      #x03 
                                      #x20 #x8e #x02
                                      #x20 #x9e #xa7 #x5))

        (buffer-x1 (binio:octet-vector 13 10 0 0 0
                                       13 20 0 0 0))
        (buffer-x2 (binio:octet-vector 8 10 ))
        )
          

    ;; Packing Tests
    ;; http://code.google.com/apis/protocolbuffers/docs/encoding.html
    
    ;; test 1 - varint
    (test-1 buffer-1 'test1 'a 150)
    ;; test 2 - string
    (test-1 buffer-2 'test2 'b "testing")
    ;; test 3 - embedded message
    (let ((value (make-instance 'test1)))
      (setf (slot-value value 'a) 150)
      (test-1 buffer-3 'test3 'c value 
              :test (lambda (a b)
                      (= (slot-value a 'a)
                         (slot-value b 'a)))))
    
    ;; test 4 - repeated unpacked varint
    (test-1 buffer-4 'test4 'd 
            (make-array 3 
                        :element-type '(signed-byte 32)
                        :initial-contents '(3 270 86942)))
    
    ;; Test x1 - repeated unpacked fixed32
    (let ((msg (make-instance 'proto-test::testx1)))
      (setf (slot-value msg 'a) (make-array 0 :adjustable t :fill-pointer t))
      (vector-push-extend 10 (slot-value msg 'proto-test::a))
      (vector-push-extend 20 (slot-value msg 'proto-test::a))
      (test-1 buffer-x1 'testx1 'a (vector 10 20)
              :test (lambda (a b) (and (= (length a) (length b))
                                       (= (aref a 0) (aref b 0))
                                       (= (aref a 1) (aref b 1))))))
    
    (test-1 buffer-x2 'testx2 'a :b)

    ;; Test float-msg
    (let* ((val 3.14s0)
           (msg (make-instance 'proto-test::float-msg :a val)))
      (format t "~&Testing float-msg~%")
      (assert (= val 
                 (slot-value (pb:unpack (pb:pack1 msg) 
                                        (make-instance 'proto-test::float-msg))
                             'proto-test::a))))
    t))


;; (defun redef-load ()
;;   (load-proto-set
;;    "/home/ntd/src/s-protobuf/tests/test.protobin"))


;; (defun test-inline ()
;;   (redef)
;;   (test))

;; (defun test-load ()
;;   (redef-load)
;;   (test))
