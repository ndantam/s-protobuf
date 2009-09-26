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

(require :s-expand)

(use-package :s-expand)
(use-package :s-expand-docbook)

(defparameter *doc*
  `(article
    (title "S-PROTOBUF")
    (:sect 1 "Introduction"
           (:sect 2 "Overview"
                  (para "This packagage, s-protobuf, provides a Common
                    Lisp implemenation for the Protocol Buffers data
                    encoding format, created by Google."))

           (:sect 2 "Getting S-PROTOBUF"
                  (programlisting 
                   ,(concatenate 'string
                                 "git clone "
                                 "http://www.prism.gatech.edu/"
                                 "~ndantam3/git/s-protobuf.git")))

           (:bsd-license 2 "Legal" 
                         "2008" "Google Inc." 
                         "2009" "Georgia Tech Research Corporation"))
    (:sect 1 "Implementation Notes"
           (:sect 2 "Details"
                  (itemizedlist
                   (listitem (para "We don't parse .proto files or use
                               protoc to generate lisp code
                               (leave C++ to the masochists), but
                               rather read in the protobuf-encoded
                               FileDescriptorSet output of protoc and
                               compile that."))
                   (listitem (para "The varint handling code supports
                              arbitrary precision."))
                   (listitem (para " We don't track unknown fields"))))
           (:sect 2 "Incompatibilities"
                  (para "Varints are strictly unsigned (svarints are
                    signed).  Cutting things off at 10 bytes and
                    making it negative is a kludge."))
           (:sect 2 "Missing Features"
                  (itemizedlist
                   (listitem (para "Skipping Unknown Fields"))
                   (listitem (para "Default Values"))
                   (listitem (para "Packages"))
                   (listitem (para "Trackng Unknown Fields"))
                   (listitem (para "Support for (#-sbcl) -- only
                                    problem area is floating-point
                                    coding"))
                   (listitem (para "Extensions"))
                   (listitem (para "Services"))
                   (listitem (para "Packed length-delimited
                                    fields (Google may not support it,
                                    but we could)")))))
    (:sect 1 "Usage"
           (para "Coming Soon"))))



(defun expand-inline ()
  (s-expand t *doc* :transform-alist *docbook-transform-alist*))

(defun expand ()
  (s-expand-file "manual.xml" *doc*
                 :doctype-string
                 (concatenate 'string
                              "<!DOCTYPE article "
                              "PUBLIC \"-//OASIS//DTD DocBook XML V4.4//EN\" "
                              "\"http://docbook.org/xml/4.2/docbookx.dtd\">")
                 :if-exists :supersede
                 :transform-alist *docbook-transform-alist*))