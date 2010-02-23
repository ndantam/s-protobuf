(asdf:operate 'asdf:load-op :s-protobuf)
(load "proto-test.lisp")

(in-package :proto-test)

(redef-load)
(test)

(sb-ext:quit)
