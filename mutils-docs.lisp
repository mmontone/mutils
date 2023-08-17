;;; mutils-docs --- Documentation generator for mutils.

;; Copyright (C) 2023 Mariano Montone. All rights reserved.

;; This work is licensed under the terms of the MIT license.  
;; For a copy, see <https://opensource.org/licenses/MIT>.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Version: 0.1
;; Requires: mutils, simple-doc

;;; Commentary:

;; Documentation generator for mutils.

;;; Code:

(require :mutils)
(require :simple-doc)

(defpackage :mutils-docs
  (:use :cl)
  (:export
   #:generate-docs))

(in-package :mutils-docs)

(defun generate-module-docs (module-details pathname)
  (with-open-file (f pathname :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create
                              :external-format :utf-8)
    (format f "# ~a~%~%" (getf module-details :name))
    (write-string (getf module-details :description) f)
    (format f "~%~%[[source code]](../~a.lisp)~%~%" (getf module-details :name))
    (dolist (property (getf module-details :properties))
      (format f "- **~a**: ~a~%" (car property) (cdr property)))
    (terpri f)
    (write-string (getf module-details :commentary) f)
    (terpri f) (terpri f)
    (require (getf module-details :name))
    (simple-doc:generate-markdown-doc
     f (intern (string-upcase (getf module-details :name)) :keyword)
     :output-undocumented t
     :include nil)))

(defun generate-readme ()
  "Generate a README file with information about available modules."
  (let ((output-file (asdf:system-relative-pathname :mutils "README.md"))
        (modules-details (mutils:list-modules :details)))
    (with-open-file (f output-file :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create
                                   :external-format :utf-8)
      (write-string (alexandria:read-file-into-string (asdf:system-relative-pathname :mutils "README.base.md")) f)
      (terpri f) (terpri f)
      (write-line "## List of modules" f)
      (terpri f)
      (dolist (module-details modules-details)
        (format f  "* [~a](docs/~a.md) - ~a~%"
                (getf module-details :name)
                (getf module-details :name)
                (getf module-details :description))))))

(defun generate-docs ()
  "Generate library docs."
  (generate-readme)
  (dolist (module-details (mutils:list-modules :details))
    (format t "Generating docs for module: ~a~%" (getf module-details :name))
    (generate-module-docs module-details
                          (asdf:system-relative-pathname :mutils
                                                         (format nil "docs/~a.md" (getf module-details :name))))))

(provide :mutils-docs)
