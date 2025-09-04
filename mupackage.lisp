(defpackage :mupackage
  (:use :cl)
  (:export #:define-package-mixin
           #:define-package
           #:define-read-context
           #:with-read-contexts))

(in-package :mupackage)

(defvar *package-mixins* (make-hash-table))

(defmacro define-package-mixin (name &rest options)
  `(setf (gethash ',name *package-mixins*)
         ',options))

(defun process-option (option)
  (case (car option)
    (:mixin (multiple-value-bind (mixin-options found-p)
                (gethash (cadr option) *package-mixins*)
              (when (not found-p)
                (error "Package mixin not found: ~s" (cadr option)))
              mixin-options))
    (:mixins (apply #'append (mapcar (lambda (mixin)
                                       (process-option (list :mixin mixin)))
                                     (cdr option))))
    (t (list option))))

(defmacro define-package (name &rest options)
  `(defpackage ,name
     ,@(apply #'append (mapcar #'process-option options))))
