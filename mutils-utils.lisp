;;; mutils-utils --- General purpose utilities.

;; Copyright (C) 2023 Mariano Montone. All rights reserved.

;; This work is licensed under the terms of the MIT license.  
;; For a copy, see <https://opensource.org/licenses/MIT>.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Version: 0.1
;; Requires: mutils

;;; Commentary:

;; General purpose utilities.

;;; Code:

(defpackage mutils-utils
  (:use :cl)
  (:export
   #:condp
   #:with-output-to-destination))

(in-package :mutils-utils)

(defmacro condp (predicate &body clauses)
  "COND using PREDICATE."
  (let ((pred (gensym)))
    `(let ((,pred ,predicate))
       (cond
         ,@(loop for clause in clauses
                 collect `((funcall ,pred ,(car clause))
                           ,@(cdr clause)))))))

(defun call-with-output-to-destination (destination function &rest args)
  "Evaluate FUNCTION with a stream created from DESTINATION as argument.
If DESTINATION is a pathname, then open the file for writing. ARGS are used in the OPEN call.
If it is a string with a fill-pointer, WITH-OUTPUT-TO-STRING is used to create a stream for it.
If it is a stream, then it is used as it is.
If it is NIL, then WITH-OUTPUT-TO-STRING is used to create the stream.
If it is T, then *STANDARD-OUTPUT* is used for the stream."
  (etypecase destination
    (pathname
     (let ((stream (apply #'open destination :direction :output args)))
       (unwind-protect
            (funcall function stream)
         (close stream))))
    (string
     (assert (array-has-fill-pointer-p destination)
             nil "Destination string doesn't have a fill-pointer")
     (let ((result nil))
       (with-output-to-string (stream destination)
         (setq result (funcall function stream)))
       result))
    (stream
     (funcall function destination))
    (null
     (with-output-to-string (stream)
       (funcall function stream)))
    ((eql t)
     (funcall function *standard-output*))))
    
(defmacro with-output-to-destination ((var destination &rest args) &body body)
  "Evaluate BODY with VAR bound to a stream created from DESTINATION.
If DESTINATION is a pathname, then open the file for writing. ARGS are used in the OPEN call.
If it is a string with a fill-pointer, use WITH-OUTPUT-TO-STRING to create a stream for it.
If it is a stream, then it is used as it is.
If it is NIL, then WITH-OUTPUT-TO-STRING is used to create the stream.
If it is T, then *STANDARD-OUTPUT* is used for the stream."
  `(call-with-output-to-destination ,destination (lambda (,var) ,@body) ,@args))

(provide :mutils-utils)
