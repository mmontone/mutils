;;; dynamic-functions --- Implementation of functions dynamically bound.
;;
;; Copyright (C) 2026 Mariano Montone. All rights reserved.
;;
;; This work is licensed under the terms of the MIT license.
;; For a copy, see <https://opensource.org/licenses/MIT>.
;;
;; Author: Mariano Montone
;; Version: 0.1
;;
;;; Commentary:
;;
;; Implementation of functions dynamically bound. The current implementation of the function depends on the current context.
;;
;; ## Usage
;;
;; Define a function as dynamic:
;;
;;     (defdynfun foo (x)
;;       x)
;;
;; Then change its implementation for a certain context:
;;
;;     (dflet ((foo (x) (1+ x)))
;;       (foo))
;;
;; ## Example
;;
;; Let's define another function BAR that calls the dynamic function FOO defined above:
;;
;;     (defun bar () (foo 1))
;;
;; Note that BAR is not dynamic, but calls FOO which is dynamic.
;; Now see result of dynamic function binding:
;;
;;     (bar) ;; => 1
;;     (dflet ((foo (x) (1+ x)))
;;        (bar)) ;; => 2
;;     (bar) ;; => 1

(defpackage :dynamic-functions
  (:use :cl)
  (:export #:define-dynfun
           #:with-dynfun-context
           #:*dynfun-context*
           #:dflet
           #:dflambda)
  (:documentation "Dynamic functions"))

(in-package :dynamic-functions)

(defvar *dynfun-context* nil)

(defmacro defdynfun (fname args &body body)
  `(defun ,fname ,args
     (let ((fun (cdr (assoc ',fname *dynfun-context*))))
       (flet ((default-fun ()
                ,@body))
         (if fun
             (funcall fun ,@args)
             (default-fun))))))

(defmacro dflet (fbindings &body body)
  `(let ((*dynfun-context* (list*
                            ,@(mapcar (lambda (fbinding)
                                        (destructuring-bind (fname args &body body) fbinding
                                          `(cons ',fname
                                                 (lambda ,args ,@body))))
                                      fbindings)
                            *dynfun-context*)))
     ,@body))

(defmacro dflambda (args &body body)
  (alexandria:with-unique-names (dynfun-context)
    ;; bind current dynamic context to a lexical variable so that we can restore it from inside the lambda
    `(let ((,dynfun-context *dynfun-context*))
       (lambda ,args
         ;; restore the dynamic context from the lexical variable
         (let ((*dynfun-context* ,dynfun-context))
           ,@body)))))
