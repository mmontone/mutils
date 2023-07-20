;; Copyright (c) 2022 Mariano Montone

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

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
