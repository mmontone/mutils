(defpackage :dynamic-functions
  (:use :cl)
  (:export #:define-dynfun
           #:with-dynfun-context
           #:*dynfun-context*
           #:dflet
           #:dflambda))

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

;; test

(defdynfun dynfun-test-1 (x y)
  (+ x y))

(dynfun-test-1 1 2)

(dflet ((dynfun-test-1 (x y)
                       (* x y)))
  (dynfun-test-1 1 2))

(progn
  (print (dynfun-test-1 1 2))
  (dflet ((dynfun-test-1 (x y)
                         (* x y)))
    (print (dynfun-test-1 1 2)))
  (print (dynfun-test-1 1 2)))


;; see that dflambda is needed:
(let (fn)
  (dflet ((dynfun-test-1 (x y)
                         (* x y)))
    (setf fn
          (lambda ()
            (dynfun-test-1 1 2))))
  (funcall fn))

;; correct version using dflambda:
(let (fn)
  (dflet ((dynfun-test-1 (x y)
                         (* x y)))
    (setf fn
          (dflambda ()
            (dynfun-test-1 1 2))))
  (funcall fn))
