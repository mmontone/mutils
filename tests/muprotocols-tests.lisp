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

(typep *standard-output* '(and stream (implements my-protocol)))

(declaim (ftype (function ((implements my-protocol) &rest t) t)
                my-func))
(defun my-func (object &rest args)
  (foo object args))

(my-func "hello")

;; =>
;; The value
;;   "hello"
;; is not of type
;;   (satisfies
;;    muprotocols-tests::implements-my-protocol-protocol-p)

(my-func *standard-output*)

