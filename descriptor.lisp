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


;; file-descriptor-set
;;
;; This is a hand translation of google's descriptor.proto by Kenton
;; Varda in order to bootstrap our CL implementation.
;;
;; Author: Neil T. Dantam


(defpackage :protocol-buffer-descriptor
  (:nicknames :proto-desc)
  (:use :cl :protoc :pb))

(in-package :proto-desc)


;(def-proto-msg 

(def-proto-msg descriptor-proto-exentsion-range
  (field start :int32 1 :optional t)
  (field end :int32 2 :optional t))

(def-proto-msg descriptor-proto
  (field name :string 1 :optional t)
  (field field field-descriptor-proto 2 :repeated t)
  (field extension field-descriptor-proto 6 :repeated t)
  (field nested-type descriptor-proto 3 :repeated t)
  (field enum-type enum-descriptor-proto 4 :repeated t)
  (field extension-range descriptor-proto-extension-range 5 
         :repeated t)
  (field options message-options 7 :optional t))

(def-proto-msg file-descriptor-proto
  (field name :string 1 :optional t)
  (field package :string 2 :optional t)
  (field dependency :string 2 :repeated t)
  (field message-type descriptor-proto 4 :repeated t)
  (field enum-type enum-descriptor-proto 5 :repeated t)
  (field service service-descriptor-proto 6 :repeated t)
  (field extension field-descriptor-proto 7 :repeated t)
  (field options file-options 8 :repeated t))


(def-proto-msg  file-descriptor-set
  (field file file-descriptor-proto 1 :repeated t))
