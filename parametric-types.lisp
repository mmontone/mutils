;;; parametric-types --- Some parametric types for CL (an experiment).

;; Copyright (C) 2023 Mariano Montone. All rights reserved.

;; This work is licensed under the terms of the MIT license.
;; For a copy, see <https://opensource.org/licenses/MIT>.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Version: 0.1

;;; Commentary:

;; Provides some parametric types, like alist-of, list-of and cons-of. This is just an experiment, so don't expect the best of the implementations.

;; ## Usage:

;; Use the parametric types in your functions:
;;
;;     (declaim (ftype (function ((alist-of symbol string)) t) foo))
;;

;;; Code:

(defpackage :parametric-types
  (:use :cl)
  (:export
   #:alist
   #:list-of
   #:alist-of
   #:cons-of))

(in-package :parametric-types)

;; from trivial-types library:

(defvar *standard-optimize-qualities*
  '((speed 3)
    (safety 0)
    (space 0)
    (debug 1)
    (compilation-speed 0)))

(declaim (inline proper-list-p
                 property-list-p
                 association-list-p
                 tuplep))

(defmacro %proper-list-p (var &optional (element-type '*))
  `(loop
     (typecase ,var
       (null (return t))
       (cons (if (or ,(eq element-type '*)
                     (typep (car ,var) ,element-type))
                 (setq ,var (cdr ,var))
                 (return)))
       (t    (return)))))

(defun proper-list-p (object)
  "Returns true if OBJECT is a proper list.

Examples:

    (proper-list-p 1) => NIL
    (proper-list-p '(1 . 2)) => NIL
    (proper-list-p nil) => T
    (proper-list-p '(1 2 3)) => T"
  (declare (optimize . #.*standard-optimize-qualities*))
  (%proper-list-p object))

(deftype proper-list (&optional (element-type '*))
  "Equivalent to `(and list (satisfies proper-list-p))`. ELEMENT-TYPE
is just ignored.

Examples:

    (typep '(1 2 3) '(proper-list integer)) => T
    (typep '(1 2 3) '(proper-list string)) => T"
  (declare (ignore element-type))
  '(and list (satisfies proper-list-p)))

(defun property-list-p (object)
  "Returns true if OBJECT is a property list.

Examples:

    (property-list-p 1) => NIL
    (property-list-p '(1 2 3)) => NIL
    (property-list-p '(foo)) => NIL
    (property-list-p nil) => T
    (property-list-p '(foo 1)) => T
    (property-list-p '(:a 1 :b 2)) => T"
  (declare (optimize . #.*standard-optimize-qualities*))
  (typecase object
    (null t)
    (cons
     (loop
       (if (null object)
           (return t)
           (let ((key (car object))
                 (next (cdr object)))
             (if (or (not (symbolp key))
                     (not (consp next)))
                 (return)
                 (setq object (cdr next)))))))))

(deftype property-list (&optional (value-type '*))
  "Equivalent to `(and list (satisfies
property-list-p))`. VALUE-TYPE is just ignored.

Examples:

    (typep '(:a 1 :b 2) '(property-list integer)) => T
    (typep '(:a 1 :b 2) '(property-list string)) => T"
  (declare (ignore value-type))
  '(and list (satisfies property-list-p)))

(defun association-list-p (var)
  "Returns true if OBJECT is an association list.

Examples:

    (association-list-p 1) => NIL
    (association-list-p '(1 2 3)) => NIL
    (association-list-p nil) => T
    (association-list-p '((foo))) => T
    (association-list-p '((:a . 1) (:b . 2))) => T"
  (declare (optimize . #.*standard-optimize-qualities*))
  (%proper-list-p var 'cons))

(deftype association-list (&optional (key-type '*) (value-type '*))
  "Equivalent to `(proper-list (cons KEY-TYPE VALUE-TYPE))`. KEY-TYPE
and VALUE-TYPE are just ignored.

Examples:

    (typep '((:a . 1) (:b . 2)) '(association-list integer)) => T
    (typep '((:a . 1) (:b . 2)) '(association-list string)) => T"
  `(proper-list (cons ,key-type ,value-type)))

(defun tuple (&rest args)
  "Exactly same as LIST."
  (declare (optimize . #.*standard-optimize-qualities*))
  args)

(defun tuplep (object)
  "Returns true if OBJECT is a tuple, meaning a proper list.

Examples:

    (tuplep 1) => NIL
    (tuplep '(1 . 2)) => NIL
    (tuplep nil) => T
    (tuplep '(1 2 3)) => T"
  (declare (optimize . #.*standard-optimize-qualities*))
  (%proper-list-p object))

;; types

(deftype cons-of (x y)
  (let* ((type-predicate-name (gensym (format nil "CONS-OF-~A-~A-" x y))))
    (setf (symbol-function type-predicate-name)
          (lambda (cons) (and (typep (car cons) x)
                         (typep (cdr cons) y))))
    `(and cons (satisfies ,type-predicate-name))))

(deftype list-of (elem-type)
  (let* ((type-predicate-name (gensym (format nil "LIST-OF-~A-" elem-type))))
    (setf (symbol-function type-predicate-name)
          (lambda (l) (every (lambda (e) (typep e elem-type)) l)))
    `(and list (satisfies ,type-predicate-name))))

(deftype alist ()
  `(and list (satisfies association-list-p)))

(deftype alist-of (key-type value-type)
  `(list-of (cons-of ,key-type ,value-type)))

(provide :parametric-types)
