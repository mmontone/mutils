;;; compiler-info --- Provides compiler info (specially from declarations) in a portable way.

;; Copyright (C) 2023 Mariano Montone. All rights reserved.

;; This work is licensed under the terms of the MIT license.  
;; For a copy, see <https://opensource.org/licenses/MIT>.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Version: 0.1

;;; Commentary:

;; Provides compiler info (specially from declarations) in a portable way.

;;; Code:

#+sbcl(require :SB-CLTL2)

(defpackage :compiler-info
  (:use :cl)
  (:export #:function-info
           #:variable-info
           #:function-type
           #:variable-type
           #:function-name))

(in-package :compiler-info)

(declaim (ftype (function ((or symbol function)) t) function-type variable-type))

(declaim (ftype (function (function) symbol) function-name))
#+sbcl
(defun function-name (function)
  (sb-impl::%fun-name function))

#+sbcl
(defun function-type (fname)
  (sb-introspect:function-type fname))

#+sbcl
(defun variable-type (varname)
  (cdr (assoc 'type (third (multiple-value-list (sb-cltl2:variable-information varname))))))

#+sbcl
(defun function-info (fname)
  (sb-cltl2:function-information fname))

#+sbcl
(defun variable-info (varname)
  (sb-cltl2:variable-information varname))

#+ccl
(defun variable-info (varname)
  (ccl::variable-information varname))

#+ccl
(defun function-info (fname))

#+ccl
(defun function-type (fname)
  (ccl::find-ftype-decl fname))

;; Utils

(declaim (ftype (function (list &optional (member declaim proclaim declare)) list) declare-derived-types))
(defun declare-derived-types (symbols &optional (form 'declaim))
  "Returns a DECLAIM form declaring the derived types of SYMBOLS."
  (let ((types nil))
    (dolist (symbol symbols)
      (when (variable-type symbol)
        (push `(type ,(variable-type symbol) ,symbol) types))
      (when (function-type symbol)
        (push `(ftype ,(function-type symbol) ,symbol) types)))
    (if (eql form 'proclaim)
        (mapcar (lambda (type)
                  `(proclaim ',type))
                types)
        ;; else
        `(,form ,@types))))

(declaim (ftype (function (package) list) list-package-external-symbols))
(defun list-package-external-symbols (package)
  (let ((symbols nil))
    (do-external-symbols (var package)
      (push var symbols))
    symbols))

(declaim (ftype (function (package) list) list-package-internal-symbols))
(defun list-package-internal-symbols (package)
  (let ((symbols nil))
    (do-symbols (var package)
      (when (eql (symbol-package var) package)
        (push var symbols)))
    symbols))

;; (declare-derived-types (list-package-external-symbols *package*))

(provide :compiler-info)
