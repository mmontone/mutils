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
   #:with-output-to-destination
   #:with-auto-gensym
   #:with-retry-restart
   #:condition-message
   #:with-accessors*))

(in-package :mutils-utils)

(defmacro condp (predicate &body clauses)
  "COND using PREDICATE.

This macro allows the implementation of \"custom pattern matchers\",
with the resulting code being easy to read.

For example:

    (defun object-kind (obj)
      (cond
        ((typep obj 'number) \"number\")
        ((typep obj 'string) \"string\")
        ((typep obj 'hashtable) \"map\")
        ((typep obj 'package) \"module\")
        ((typep obj 'class) \"type\")))

can be rewritten as:

    (defun object-kind (obj)
      (condp (alexandria:curry #'typep obj)
        ('number \"number\")
        ('string \"string\")
        ('boolean \"boolean\")
        ('hash-table \"map\")
        ('package \"module\")
        ('class \"type\")))

ALEXANDRIA:CURRY and ALEXANDRIA:RCURRY are very useful in companion with this macro.

A regular expression matcher:

    (condp (alexandria:rcurry #'ppcre:scan \"my-string\")
       (\"^foo\" :foo)
       (\"^bar\" :bar)
       (\"^my.*\" :mine)
       (\"^mi.*\" :mio))

A string matcher:

    (condp (alexandria:curry #'string= \"some\")
       (\"foo\" :foo)
       (\"bar\" :bar)
       (\"some\" :some))
"
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

- If DESTINATION is a pathname, then open the file for writing. ARGS are used in the OPEN call.
- If it is a string with a fill-pointer, use WITH-OUTPUT-TO-STRING to create a stream for it.
- If it is a stream, then it is used as it is.
- If it is NIL, then WITH-OUTPUT-TO-STRING is used to create the stream.
- If it is T, then *STANDARD-OUTPUT* is used for the stream."
  `(call-with-output-to-destination ,destination (lambda (,var) ,@body) ,@args))

(defun auto-gensym-p (thing)
  (and (symbolp thing)
       (char= #\#
              (aref (symbol-name thing)
                    (1- (length (symbol-name thing)))))))

(defun auto-gensym-name (symbol)
  (subseq (symbol-name symbol) 0 (1- (length (symbol-name symbol)))))

(defun insert-gensyms (form)
  (let (gensyms)
    (labels ((get-gensym (symbol)
               (if (assoc symbol gensyms)
                   (cdr (assoc symbol gensyms))
                   (let ((auto-gensym (gensym (auto-gensym-name symbol))))
                     (push (cons symbol auto-gensym) gensyms)
                     auto-gensym)))
             (replace-auto-gensyms (%form)
               (if (atom %form)
                   (if (auto-gensym-p %form)
                       (get-gensym %form)
                       %form)
                   (mapcar #'replace-auto-gensyms %form))))
      (replace-auto-gensyms form))))

(defmacro with-auto-gensym (body)
  "Replace auto-gensym symbols in BODY.

auto-gensym symbols are those that end with a # character.

Every symbol matching a unique foo# symbol within a syntax quoted form will be replaced with the same generated symbol.

Examples:

    (defmacro auto-gensym-test (x)
      (with-auto-gensym
        `(let ((x# ,x))
           (+ x# 22))))

Macroexpansion of `(auto-gensym-test 44)`:

    (LET ((#:X1 44))
      (+ #:X1 22))
"
  (insert-gensyms body))

(defun call-with-retry-restart (thunk msg &rest args)
  (loop
    (restart-case (return (funcall thunk))
      (retry nil :report
        (lambda (s)
          (apply #'format s msg args))))))

(defmacro with-retry-restart ((&optional (msg "Retry.") &rest args) &body body)
  "Setup a RETRY restart for evaluating BODY."
  (check-type msg string)
  `(call-with-retry-restart (lambda () ,@body) ,msg ,@args))


(declaim (ftype (function (condition) string) condition-message))
(defun condition-message (condition)
  "Get the descriptive message of CONDITION."
  (with-output-to-string (s)
    (write condition :escape nil :stream s)))

(defmacro with-accessors* (bindings instance &body body)
  `(cl:with-accessors ,(loop for binding in bindings
                             collect (if (symbolp binding)
                                         (list binding binding)
                                         binding))
       ,instance
     ,@body))

(provide :mutils-utils)
