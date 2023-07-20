(defpackage :mutils
  (:use :cl)
  (:export :condp))

(in-package :mutils)

(pushnew
 (asdf:system-source-directory :mutils)
 directory-module-loader:*module-directories*)

(defmacro condp (predicate &body clauses)
  "COND using PREDICATE."
  (let ((pred (gensym)))
    `(let ((,pred ,predicate))
       (cond
	 ,@(loop for clause in clauses
		 collect `((funcall ,pred ,(car clause))
			   ,@(cdr clause)))))))
 
