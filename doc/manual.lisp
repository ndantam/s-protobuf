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
                  (para "This package, S-PROTOBUF, provides a Common
                    Lisp implementation for the" ((ulink url
                    "http://code.google.com/apis/protocolbuffers/docs/overview.html")
                                                 "Protocol Buffers")
                    "data encoding format,
                    created by Google."))
           (:sect 2 "Status"
                  (para "S-PROTOBUF supports message packing and
                  unpacking using all the specified data types,
                  repeated fields, and nested messages.  Packed
                  repeated fields may not quite be there yet. There is
                  no support for Extensions or Services."))
           (:sect 2 "Getting S-PROTOBUF"
                  (programlisting 
                   ,(concatenate 'string
                                 "git clone "
                                 "http://www.prism.gatech.edu/"
                                 "~ndantam3/git/s-protobuf.git")))
           (:sect 2 "Authors"
                  (para "S-PROTOBUF was written by Neil
                  T. Dantam ({first-initial,middle-initial,last-initial}@gatech.edu).
                  Zach Beane provided some helpful suggestions to
                  clean up parts of the code.  The original Protocol
                  Buffers design was developed internally at Google by
                  a number of people.  Current efforts seem to be led
                  by Kenton Varda ({firstname}@google.com)."))
           (:sect 2 "Alternatives"
                  (para "If S-PROTOBUF or Protocol Buffers in general
                  doesn't meet your needs, here are some things that
                  might")
                  (itemizedlist
                   (listitem 
                    (para 
                     ((ulink 
                       url 
                       "http://code.google.com/p/protobuf/wiki/ThirdPartyAddOns")
                      "Other Protobuf Implementations")))

                   (listitem (para ((ulink url "http://www.asn1.org/")
                                    "ASN.1")))
                   (listitem (para ((ulink url 
                                           "http://developers.facebook.com/thrift/")
                                    "Facebook Thrift")))
                   (listitem 
                    (para 
                     ((ulink 
                       url 
                       "http://en.wikipedia.org/wiki/External_Data_Representation")
                      "XDR")))
                   ))
           (:bsd-license 2 "Legal" 
                         "2008" "Google Inc." 
                         "2009-2010" "Georgia Tech Research Corporation"))
    (:sect 1 "Implementation Notes"
           (:sect 2 "Details"
                  (itemizedlist
                   (listitem (para "We don't parse .proto files or use
                               protoc to generate lisp code
                               (leave C++ to the masochists), but
                               rather read in the protobuf-encoded
                               FileDescriptorSet output of" (command
                               protoc) "and compile that."))
                   (listitem (para "The varint handling code supports
                              arbitrary precision."))
                   (listitem (para " We don't track unknown fields"))))
           (:sect 2 "Incompatibilities"
                  (para "Varints are strictly unsigned (svarints are
                    signed).  Cutting things off at 10 bytes and
                    making it negative is a kludge."))
           (:sect 2 "Bugs"
                  (itemizedlist
                   (listitem (para "Insufficient testcases"))
                   (listitem (para "Packages -- sort of supported, but probably not too well"))
                   (listitem (para "Required/Optional fields -- we
                     don't currently care if required fields aren't
                     there"))))
           (:sect 2 "Missing Features"
                  (itemizedlist
                   (listitem (para "Skipping Unknown Fields"))
                   (listitem (para "Default Values"))
                   (listitem (para "Tracking Unknown Fields"))
                   (listitem (para "Better support for (#-sbcl).  The
                                    basic testcases do run on CLISP.
                                    UTF-8 only works on SBCL."))
                   (listitem (para "Extensions"))
                   (listitem (para "Services"))
                   ))
           (:sect 2 "Possible Improvements"
                  (itemizedlist
                   (listitem (para "Some refactoring of protoc.lisp would help"))
                   (listitem (para "ASDF Integration"))
                   (listitem (para "Packed length-delimited
                                    fields (Google may not support it,
                                    but we could)")))))
    (:sect 1 "Usage"
           (:sect 2 "Compiling Encoders"
                  (:sect 3 "Using .proto files"
                         (para "S-PROTOBUF reads in the binary output of"
                               (command protoc)".  Thus, you should
                           first run your .proto file
                           through" (command protoc) "using the -o
                           option: ")
                         (programlisting "$ protoc -omyfile.protobin myfile.proto")
                         (para "Then you can use the macro
                           function " (function "protoc:load-proto-set") "
                           which reads in the .protobin and expands
                           into the definitions for classes,
                           functions, and methods.")
                         (programlisting "(require :s-protobuf)"
                                         "(protoc:load-proto-set \"myfile.protobin\")")
                         (para "The class will be put into the package
                           listed in .proto file, or CL-USER is none
                           was specified.  Nested packages will
                           probably not work very well."))
                  (:sect 3 "Writing protobuf's as S-Expressions"
                         (para "If curly braces make you angry, you
                           can also write the protocol buffer
                           definitions as S-Expressions.  Here is
                           documentation by example:")
                         (programlisting 
                          "(require :s-protobuf)"
                          ,(string #\Newline)
                          ";; A simple message"
                          "(protoc:def-proto-msg test1"
                          "  (field a :int32 1))"
                          ,(string #\Newline)
                          ";; A nested message"
                          "(protoc:def-proto-msg test3"
                          "  (field c test1 3))"
                          ,(string #\Newline)
                          ";; repeated fields"
                          "(protoc:def-proto-msg test4"
                          "  (field d :int32 4 :repeated t :packed nil))"
                          ,(string #\Newline)
                          ";; enums"
                          "(protoc:def-proto-msg testx2"
                          "  (enum e (:a 0) (:b 10) (:c 20))"
                          "  (field a testx2-e 1 :repeated nil :packed nil))"
                          )
                         (para "The class will be put into whatever
                           package the symbol for it's name is in.")
                         (para "If you want some more details, you can
                           try macroexpanding " (function
                           load-proto-set) ".")))
           (:sect 2 "Using Encoders"
                  (para "Each protobuf message will be compiled into a
                    class.  Identifiers are mangled into a lispy
                    form (upcased symbols, hyphenated words).  Enums
                    will be translated to and from keyword symbols on
                    encoding and decoding.  Repeated fields are
                    vectors which are are simple-array's when
                    possible (packed types with fixed bit size).")
                  (itemizedlist
                   (listitem (:docfun "PB:PACK" 
                                      "Generic function, packs
                                      protobuf into an octet buffer"
                                      (packed-size buffer) (protobuf "The object to
                                      encode") &optional buffer
                                      start))
                   (listitem (:docfun "PB:UNPACK" 
                                      "Generic function, unpacks a
                                        protobuf object from an octet
                                        buffer"
                                      (protobuf bytes-read) 
                                      buffer protobuf &optional start end))
                   (listitem (:docfun "PB:PACKED-SIZE"  
                                      "Generic function, number of
                                      octets required to encode
                                      protobuf" size protobuf)) )))
    (:sect 1 "Wire Format"
           (:sect 2 "Varints"
                  (para "Varints are a space-efficient encoding of
                    integers.  Each octet contains the next 7
                    little-endian bits of the integer in the low bits
                    of the octet with the high bit of the octet set if
                    another octet is required.  More formally, bits 0
                    through 6 of the nth octet of the encoded data
                    contains little-endian bits 7*n through 7*n+7 of
                    the integer, and bit 7 of the nth octet is set if
                    and only if there is a n+1'th octet.")

                  (para "While this encoding scheme works well for
                    unsigned integers to arbitrary precision, negative
                    integers in two's complement would create an
                    non-terminating sequence.  To resolve this
                    problem, signed integers are mapped to unsigned
                    integers using a zig-zag encoding.  This sequence
                    follows the patter of 0->0, -1->1, 1->2, -2->3,
                    2->4... This mapping can be expressed as:")
                  (programlisting
                   "zigzag(n) = 2*abs(n) - sign(n) * ( (sign(n) - 1) &gt;&gt; 1)"
                   "unzigzag(z) = ( (n+(n&amp;1)) &gt;&gt;> 1 ) * ( 1 - 2*(n&amp;1))")
                  )
           (:sect 2 "Primitive Data Types"
                  (para "All numeric types are encoded in
                  little-endian byte order.")
                  (variablelist
                   (varlistentry 
                    (term "double")
                    (listitem (para "IEEE754 double precision floating
                    point")))
                   (varlistentry
                    (term "float")
                    (listitem (para "IEEE754 single precision floating
                    point")))
                   (varlistentry 
                    (term "int32")
                    (listitem (para "unsigned varint, maximum 32 bits
                    decoded precision")))
                   (varlistentry 
                    (term "int64")
                    (listitem (para "unsigned varint, maximum 64 bits
                               decoded precision" )))
                   (varlistentry 
                    (term "uint32")
                    (listitem (para "unsigned varint, maximum 32 bits
                    decoded precision")))
                   (varlistentry 
                    (term "uint64")
                    (listitem (para "unsigned varint, maximum 64 bits
                    decoded precision")))
                   (varlistentry
                    (term "sint32")
                    (listitem (para "signed varint, maximum 32 bits
                    decoded precision")))
                   (varlistentry
                    (term "sint64")
                    (listitem (para "signed varint, maximum 64 bits
                    decoded precision")))
                   (varlistentry
                    (term "fixed32")
                    (listitem (para "unsigned 32 bit integer, encoded
                      as four little-endian bytes")))
                   (varlistentry
                    (term "fixed64")
                    (listitem (para "unsigned 64 bit integer, encoded as eight little-endian
                      byte")))
                   (varlistentry
                    (term "sfixed32")
                    (listitem (para "signed 32 bit integer, encoded as four little-endian
                     byte")))
                   (varlistentry
                    (term "sfixed64")
                    (listitem (para "signed 64 bit integer, encoded as eight little-endian
                      byte")))
                   (varlistentry
                    (term "bool")
                    (listitem (para "varint-encoded 0 or 1")))
                   (varlistentry
                    (term "string")
                    (listitem (para "A text-string encoded as a
                      sequence of UTF8 bytes, prefixed with the number
                      of bytes encoded as an unsigned varint")))
                   (varlistentry
                    (term "bytes")
                    (listitem (para  "A raw sequence of bytes")))
                   )
                  )
           (:sect 2 "Field Encoding"
                  (para "Each field of a message is encoded as a tag
                    followed by the encoded field data.  The tag is
                    the varint encoding of an integer whose lower
                    three bytes are a type code and whose upper bytes
                    are the defined position of the field.  Note that
                    to recover the position from the tag, the tag must
                    be downshifted three bits.")
                  (para "The purpose of the typecode is to indicate
                   the size of the field so that conforming
                   applications who have a message definition that is
                   unaware of that field can skip over it and recover
                   the remainder of the message.  Conforming
                   implementations are required to skip over unknown
                   fields in this manner.")

                  (para "The following type codes are currently defined:")
                  (itemizedlist

                   (listitem (para "0: a varint"))
                   (listitem (para "1: fixed size, 64 bit"))
                   (listitem (para "5: fixed size, 32 bit"))
                   (listitem (para "2: a unsigned varint encoded
                             length followed by that many bytes"))
                   ))
           (:sect 2 "Message Encoding"
                  (para "A message is encoded as the simple
                     concatenation of its encoded fields.  There is no
                     defined whole-message type code, delimiter, or
                     size indicator; implementations should handle
                     these needs via some external mechanism."))
    )))
           


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
