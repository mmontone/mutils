(defpackage :dynamic-context
  (:use :cl)
  (:export #:*context*
           #:call-with-context
           #:with-context
           #:let-context
           #:get-context
           #:with-context-bindings))

(in-package :dynamic-context)

;; dynamic bindings without the need of creating new dynamic variables
;; dynamic bindings are bound to the context without declaration

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
