;;; debug-print --- A reader macro package for debug printing.
;;
;; Copyright (C) 2018 Satoshi Imai. All rights reserved.
;;
;; This work is licensed under the terms of the MIT license.
;; For a copy, see <https://opensource.org/licenses/MIT>.
;;
;; Author: Satoshi Imai
;; Version: 0.2
;; Requires: cl-syntax, named-readtables
;;
;;; Commentary:
;;
;; A reader macro package for debug printing.
;;
;; ## Usage
;;
;;    (debug-print:use-debug-print)
;;
;; ### Debug print
;;
;;    (defun fact (n)
;;      (if (= #>n 0)
;;          1
;;          (* n #>(fact (1- n)))))
;;
;;    (fact 10)
;;
;;    ;; N => 10
;;    ;; N => 9
;;    ;; N => 8
;;    ;; N => 7
;;    ;; N => 6
;;    ;; N => 5
;;    ;; N => 4
;;    ;; N => 3
;;    ;; N => 2
;;    ;; N => 1
;;    ;; N => 0
;;    ;; (FACT (1- N)) => 1
;;    ;; (FACT (1- N)) => 1
;;    ;; (FACT (1- N)) => 2
;;    ;; (FACT (1- N)) => 6
;;    ;; (FACT (1- N)) => 24
;;    ;; (FACT (1- N)) => 120
;;    ;; (FACT (1- N)) => 720
;;    ;; (FACT (1- N)) => 5040
;;    ;; (FACT (1- N)) => 40320
;;    ;; (FACT (1- N)) => 362880
;;
;; ### Debug push
;;
;;    (defun fact2 (n)
;;      (if (= n 0)
;;          1
;;          (* n #!(fact2 (1- n)))))
;;
;;    (fact2 10)
;;    ;; => 3628800
;;
;; ### Debug push results are stored to *dbg*
;;
;;    debug-print:*dbg*
;;    ;; => (362880 40320 5040 720 120 24 6 2 1 1)
;;
;;    ;;; Clearing debug push list
;;
;;    (debug-print:clear-dbg)
;;
;;    debug-print:*dbg*
;;    ;; => nil
;;
;; ## Configure variables
;;
;; ### Setting destination stream (default value is *standard-output*)
;;
;;    (setf debug-print:*destination* *error-output*)
;;
;; ### Setting for using DESCRIBE instead of mere value (default value is nil)
;;
;;    (setf debug-print:*use-describe* t)
;;
;;    #>'sin
;;
;;    ;; 'SIN => COMMON-LISP:SIN
;;    ;;   [symbol]
;;    ;;
;;    ;; SIN names a compiled function:
;;    ;;   Lambda-list: (NUMBER)
;;    ;;   Declared type: (FUNCTION (NUMBER)
;;    ;;                   (VALUES
;;    ;;                    (OR (SINGLE-FLOAT -1.0 1.0)
;;    ;;                        (DOUBLE-FLOAT -1.0d0 1.0d0)
;;    ;;                        (COMPLEX SINGLE-FLOAT) (COMPLEX DOUBLE-FLOAT))
;;    ;;                    &OPTIONAL))
;;    ;;   Derived type: (FUNCTION (T)
;;    ;;                  (VALUES
;;    ;;                   (OR (SINGLE-FLOAT -1.0 1.0)
;;    ;;                       (DOUBLE-FLOAT -1.0d0 1.0d0)
;;    ;;                       (COMPLEX DOUBLE-FLOAT) (COMPLEX SINGLE-FLOAT))
;;    ;;                   &OPTIONAL))
;;    ;;   Documentation:
;;    ;;     Return the sine of NUMBER.
;;    ;;   Known attributes: foldable, flushable, unsafely-flushable, movable, recursive
;;    ;;   Source file: SYS:SRC;CODE;IRRAT.LISP
;;
;;; Code:

(require :cl-syntax)
(require :named-readtables)

(defpackage debug-print
  (:use #:cl
        #:cl-syntax
        #:named-readtables)
  (:export #:debug-print
           #:debug-print-reader
           #:debug-push
           #:debug-push-reader
           #:*use-describe*
           #:*destination*
           #:*dbg*
           #:clear-dbg
           #:debug-print-syntax
           #:use-debug-print))

(in-package :debug-print)

(defvar *use-describe* nil
  "When *USE-DESCRIBE* is true, describe is used for debug prints.")

(defvar *destination* nil
  "*DESTINATION* allows to configure destination stream for debug prints.")

(defvar *dbg* nil
  "*DBG* is a list, and using DEBUG-PUSH(#!) will be pushed to this list.
To clear this variable, use the CLEAR-DBG function.")

(defun debug-print (pre-exp exp)
  (let ((*destination* (or *destination* *standard-output*)))
    (if *use-describe*
        (format *destination* "~A => ~A~%" pre-exp
                (with-output-to-string (s)
                  (describe exp s)))
        (format *destination* "~A => ~S~%" pre-exp exp)))
  exp)

(defun debug-print-reader (stream char1 char2)
  (declare (ignore char1 char2))
  (let ((read-data (read stream t nil t)))
    `(debug-print (quote ,read-data) ,read-data)))

(defun clear-dbg ()
  (setf *dbg* nil))

(defun debug-push (obj)
  (push obj debug-print:*dbg*)
  obj)

(defun debug-push-reader (stream char1 char2)
  (declare (ignore char1 char2))
  (let ((read-data (read stream t nil t)))
    `(debug-push ,read-data)))

(defsyntax debug-print-syntax
  (:merge :standard)
  (:dispatch-macro-char #\# #\> #'debug-print-reader)
  (:dispatch-macro-char #\# #\! #'debug-push-reader))

(defun use-debug-print ()
  (cl-syntax:use-syntax debug-print-syntax)
  t)

(provide :debug-print)
