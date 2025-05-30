(defpackage :mu-lambda-list-tests
  (:use :cl)
  (:shadowing-import-from
   #:mu-lambda-list
   #:defun #:lambda #:destructuring-bind))

(in-package :mu-lambda-list-tests)

(funcall
 (lambda (_x y)
   y)
 1 2)

(funcall
 (lambda (_x (_ y))
   y)
 1 (list 1 3))

(defun foo (_x)
  "foo")

(defun bar ((x &key y))
  (list x y))

(bar '(x :y y))

(defun baz ((&key z f))
  (list z f))

(baz '(:z z :f f))
