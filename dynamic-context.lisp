;;; dynamic-context --- Dynamic bindings without the need of defining new dynamic variables.
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
;; Dynamic bindings without the need of defining new dynamic variables.
;; Dynamic bindings are bound to the context without declaration.
;;
;;; Code:

(defpackage :dynamic-context
  (:use :cl)
  (:export #:*context*
           #:call-with-context
           #:with-context
           #:let-context
           #:get-context
           #:get-context*
           #:with-context-bindings))

(in-package :dynamic-context)

(defvar *context* nil)

(defun call-with-context (context func)
  (let ((*context* context))
    (funcall func)))

(defmacro with-context (context &body body)
  `(call-with-context ,context (lambda () ,@body)))

(defmacro let-context (bindings &body body)
  `(call-with-context (list* ,@(mapcar (lambda (binding)
                                         `(cons ',(first binding)
                                                ,(second binding)))
                                       bindings)
                             *context*)
                      (lambda () ,@body)))

(defmacro get-context (name &optional default)
  `(let ((binding (assoc ',name *context*)))
     (if binding
         (cdr binding)
         ,default)))

(defun get-context* (name &optional default)
  (let ((binding (assoc name *context*)))
    (if binding
        (cdr binding)
        default)))

(defmacro with-context-bindings (bindings &body body)
  `(let ,(mapcar (lambda (binding)
                   `(,binding (get-context ,binding)))
          bindings)
     ,@body))

;; (defun bar-context ()
;;   (get-context foo))

;; (defun foo-context ()
;;   (let-context ((foo 22))
;;     (bar-context)))

;; (foo-context)

(provide :dynamic-context)
