;;; auto-gensym --- Clojure style AUTO-GENSYM macro.

;; Copyright (C) 2023 Mariano Montone. All rights reserved.

;; This work is licensed under the terms of the MIT license.  
;; For a copy, see <https://opensource.org/licenses/MIT>.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Version: 0.1

;;; Commentary:

;; Clojure style AUTO-GENSYM macro.

;;; Code:

(defpackage :auto-gensym
  (:use :cl)
  (:export :with-auto-gensym)
  (:documentation "Generate symbols automatically in macros, as in Clojure."))

(in-package :auto-gensym)

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

(macroexpand '(auto-gensym-test 44)) =>
(LET ((#:X1 44))
  (+ #:X1 22))
"
  (insert-gensyms body))

(provide 'auto-gensym)
