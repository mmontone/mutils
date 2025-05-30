(mu-lambda-list:defpackage :mu-lambda-list-tests
  (:use :cl))

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

(defun destructure-with-cons ((x . y))
  (cons y x))

(destructure-with-cons (cons 1 2))

(defun destructure-with-cons-2 ((_ . x))
  x)

(destructure-with-cons-2 (cons 1 2))

(defun ignorable-arguments (x _y &optional z)
  (list x z))

(ignorable-arguments 1 2 3)

(defun ignorable-arguments-2 (x y &optional _z)
  (list x y))

(ignorable-arguments-2 1 2 3)

(defun ignorable-arguments-3 (x _y &rest _args)
  (list x))

(ignorable-arguments-3 1 2 3 4)
