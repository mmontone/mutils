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

(defmacro with-read-contexts (context-designators &body body)
  (let* ((contexts (mapcar #'find-read-context context-designators))
         (definitions (apply #'append contexts))
         (variables (remove-if-not (lambda (def-type)
                                     (eql def-type :variable))
                                   definitions
                                   :key #'car))
         (macros (remove-if-not (lambda (def-type)
                                  (eql def-type :macro))
                                definitions
                                :key #'car))
         (functions (remove-if-not (lambda (def-type)
                                     (eql def-type :function))
                                   definitions
                                   :key #'car)))
    `(symbol-macrolet
         ,(loop for var in variables
                collect (cdr var))
       (flet
           ,(loop for def in functions
                  collect `(,(cadr def) (&rest args)
                            (apply #',(caddr def) args)))
         (macrolet
             ,(loop for def in macros
                    collect `(,(cadr def) (&rest args)
                              `(,',(caddr def) ,@args)))
           ,@body)))))
