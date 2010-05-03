(asdf:operate 'asdf:load-op :s-protobuf)
(load "proto-test.lisp")

(in-package :proto-test)

(test)

#+sbcl (sb-ext:quit)
#+clisp (ext:quit)
