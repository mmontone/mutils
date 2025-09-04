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


(defvar *read-context* (make-hash-table))

(defmacro define-read-context (name &rest definitions)
  `(setf (gethash ',name *read-context*)
         ',definitions))

(defun find-read-context (name)
  (or (gethash name *read-context*)
      (error "Read context not found: ~s" name)))

(defun replace-symbols (symbols form)
  (if (atom form)
      (let ((replacement (assoc form symbols)))
        (if replacement
            (cdr replacement)
            form))
      (mapcar (lambda (x) (replace-symbols symbols x))
              form)))

(defmacro with-read-contexts (context-designators &body body)
  `(progn
     ,@(replace-symbols (find-read-context (car context-designators)) body)))
