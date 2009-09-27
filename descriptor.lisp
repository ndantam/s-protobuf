;;; -*- Lisp -*-
;; Copyright 2008, Google Inc. 
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
;; Technically, we could do some complicated bootstrapping procedure
;; wherein we use the hand-translation to load the binary version of
;; descriptor.proto and ultimately run coders derived from that, but
;; it's probably easier for everyone if we just manually keep
;; descriptor.lisp synchronized with descriptor.proto (and since
;; Protocol Buffers are designed to be Forward/Backward compatible,
;; everyone should turn out OK anyway...)
;;
;; Author: Neil T. Dantam


;;(defpackage :protocol-buffer-descriptor
  ;;(:nicknames :proto-desc)
  ;;(:use :cl :protoc :pb :binio))

;;(in-package :proto-desc)
(in-package :protoc)


(def-proto-msg uninterpreted-option-name-part
  (field name-part :string 1 :required t)
  (field is-extension :bool 2 :required t))

(def-proto-msg uninterpreted-option
  (field name uninterpreted-option-name-part 2 
         :repeated t)
  (field identifier-value :string 3 :optional t)
  (field positive-int-value :uint64 4 :optional t)
  (field negative-int-value :sint64 5 :optional t)
  (field double-value :double 6 :optional t)
  ;(field string-value :bytes :optional t)
 ) 


(def-proto-msg method-options
   (field uninterpreted-option 
         uninterpreted-option 999
         :repeated t)
  ;; extension ??
)


(def-proto-msg service-options
   (field uninterpreted-option 
         uninterpreted-option 999
         :repeated t)
  ;; extension ??
)

(def-proto-msg enum-value-options
   (field uninterpreted-option 
         uninterpreted-option 999
         :repeated t)
  ;; extension ??
)

(def-proto-msg enum-options
   (field uninterpreted-option 
         uninterpreted-option 999
         :repeated t)
  ;; extension ??
)
 

(def-proto-msg field-options
  (enum ctype
        (:cord 1)
        (:string_piece 2))
  (field ctype field-options-ctype 1 
         :optional t)
  (field packed :bool 2 :optional t)
  (field experimental-map-key :string 9
         :optional t)
  (field uninterpreted-option 
         uninterpreted-option 999
         :repeated t)
  ;; extension ??
)
  

(def-proto-msg message-options
  (field message-set-wire-format :bool
         1 :optional t)
  (field uninterpreted-option 
         uninterpreted-option 999
         :repeated t)
  ;; extension ??
)

(def-proto-msg file-options
  (field java-package :string 1 :optional t)
  (field java-outer-classname :string 8 :optional t)
  (enum optimize-mode
        (:speed 1)
        (:code-size 2))
  (field optimize-for 
         file-options-optimize-mode
         9
         :optional t)
  (field uninterpreted-option
         uninterpreted-option 999
         :repeated t)
  ;; extension??
)

(def-proto-msg method-descriptor-proto
  (field name :string 1 :optional t)
  (field input-type :string 2 :optional t)
  (field output-type :string 3
         :optional t)
  (field options method-options 4
         :optional t))

(def-proto-msg service-descriptor-proto
  (field name :string 1)
  (field method method-descriptor-proto 2 
         :repeated t)
  (field options service-options 3
         :optional t))


(def-proto-msg enum-value-descriptor-proto
  (field name :string 1 :optional t)
  (field number :int32 2 :optional t)
  (field options enum-value-options 3 :optional t))

(def-proto-msg enum-descriptor-proto
  (field name :string 1 :optional t)
  (field value enum-value-descriptor-proto 2
         :repeated t)
  (field options enum-options 3 :optional t))


(def-proto-msg field-descriptor-proto
  (enum type 
        (:double 1)
        (:float 2)
        (:int64 3)
        (:uint64 4)
        (:int32 5)
        (:fixed64 6)
        (:fixed32 7)
        (:bool 8)
        (:string 9)
        (:group 10)
        (:message 11)
        (:bytes 12)
        (:uint32 13)
        (:enum 14)
        (:sfixed32 15)
        (:sfixex64 16)
        (:sint32 17)
        (:sint64 18))
  (enum label
        (:label-optional 1)
        (:label-required 2)
        (:label-repeated 3))
  (field name :string 1 :optional t)
  (field number :int32 3 :optional t)
  (field label field-descriptor-proto-label 4
         :optional t)
  (field type field-descriptor-proto-type 5
         :optional t)
  (field type-name :string 6 :optional t)
  (field extendee :string  2 :optional t)
  (field default-value :string 7 :optional t)
  (field options field-options 8 :optional t))
        

(def-proto-msg descriptor-proto-extension-range
  (field start :int32 1 :optional t)
  (field end :int32 2 :optional t))

;; a message
(def-proto-msg descriptor-proto
  (field name :string 1 :optional t)
  (field field field-descriptor-proto 2 :repeated t)
  (field extension field-descriptor-proto 6 :repeated t)
  (field nested-type descriptor-proto 3 :repeated t)
  (field enum-type enum-descriptor-proto 4 :repeated t)
  (field extension-range descriptor-proto-extension-range 5 
         :repeated t)
  (field options message-options 7 :optional t))

;; file container
(def-proto-msg file-descriptor-proto
  (field name :string 1 :optional t)
  (field package :string 2 :optional t)
  (field dependency :string 3 :repeated t)
  (field message-type descriptor-proto 4 :repeated t)
  (field enum-type enum-descriptor-proto 5 :repeated t)
  (field service service-descriptor-proto 6 :repeated t)
  (field extension field-descriptor-proto 7 :repeated t)
  (field options file-options 8 :repeated t))


;; multi file container
(def-proto-msg  file-descriptor-set
  (field file file-descriptor-proto 1 :repeated t))

