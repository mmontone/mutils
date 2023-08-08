(defpackage :muprotocols-tests
  (:use :cl :muprotocols))

(in-package :muprotocols-tests)

(defprotocol my-protocol
  (foo (x y)
       (:documentation "Do foo"))
  (bar (w z)
       (:documentation "Do bar")))

(typep *standard-output* '(implements my-protocol))

(implement-protocol my-protocol stream
  (foo ()))

(implement-protocol my-protocol stream
  (foo ())
  (bar ())
  (baz ()))

(implement-protocol my-protocol stream
  (foo ((x stream) y)
       "foo")
  (bar ((w stream) z)))

(typep *standard-output* '(implements my-protocol))

(check-implements *standard-output* my-protocol)

(foo *standard-output* t)
