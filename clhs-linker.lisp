;;; clhs-linker --- Replace Lisp terms in a file by hyperlinks to Common Lisp HyperSpec.
;;
;; Copyright (C) 2023 Mariano Montone. All rights reserved.
;;
;; This work is licensed under the terms of the MIT license.
;; For a copy, see <https://opensource.org/licenses/MIT>.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Version: 0.1
;; Requires: mutils-utils, colorize
;;
;;; Commentary:
;;
;; Replace Lisp terms in a file by hyperlinks to Common Lisp HyperSpec.
;;
;; Supported input file types are Markdown and HTML.
;;
;;; Code:

(require :colorize)
(require :mutils-utils)

(defpackage :clhs-linker
  (:use :cl)
  (:export
   #:link-file
   #:*lisp-term-regex*))

(in-package :clhs-linker)

(defvar *lisp-term-regex* "\\b[A-Z]+\\b"
  "The regex for matching Lisp terms.")

(defun clhs-replace-with-link (match docext)
  (let ((link (clhs-lookup:symbol-lookup match)))
    (if link
        (cond
          ((member docext '("md" "markdown") :test #'equalp)
           (format nil "[~a](~a)" match link))
          ((member docext '("html") :test #'equalp)
           (format nil "<a href=\"~a\">~a</>" link match))
          (t
           (error "Can't process document with extension: ~a" docext)))
        match)))


(declaim (ftype (function (pathname &optional t) t)
                link-file))
(defun link-file (pathname &optional (destination *standard-output*))
  "Replace Lisp terms in PATHNAME by links to Common Lisp HyperSpec.

DESTINATION can be: a PATHNAME, a STRING with a FILL-POINTER, a STREAM,
NIL (writes to a string), or T (writes to *STANDARD-OUTPUT*)
.
Supported input file types are Markdown and HTML."
  (with-open-file (in pathname)
    (mutils-utils:with-output-to-destination (out destination)
      (loop :with docext := (pathname-type pathname)
            :for line := (read-line in nil :eof)
            :while (not (eql line :eof))
            :for linked-line := (ppcre:regex-replace-all *lisp-term-regex*
                                line
                                (lambda (target-string start end match-start match-end reg-start reg-end)
                                  (declare (ignore start end reg-start reg-end))
                                  (let ((match (subseq target-string match-start match-end)))
                                    (clhs-replace-with-link match docext))))
            :do (write-line linked-line out)))))

(provide :clhs-linker)
