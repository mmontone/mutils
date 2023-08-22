;;; lisp-critic-warnings --- Signal compiler warnings with lisp-critic critiques.

;; Copyright (C) 2023 Mariano Montone. All rights reserved.

;; This work is licensed under the terms of the MIT license.
;; For a copy, see <https://opensource.org/licenses/MIT>.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Version: 0.1
;; Requires: lisp-critic, compiler-hooks, alexandria

;;; Commentary:

;; Signal compiler warnings with lisp-critic critiques.
;;
;; After the module is loaded, LISP-CRITIC warnings with critiques are signaled on COMPILE-FILE calls.
;;
;; Use CRITIQUE declamations to control what packages, files, functions gets critiqued:
;;     (declaim lisp-critic-warnings:critique nil :package)
;;     (declaim lisp-critic-warnings:critique nil :file)
;;     (declaim lisp-critic-warnings:critique nil my-function)
;;
;;; Code:

(require :lisp-critic)
(require :compiler-hooks)
(require :alexandria)

(defpackage :lisp-critic-warnings
  (:use :cl)
  (:export #:*critic-warnings*
           #:critique ;; declamation
           #:*ignore-packages*
           #:*ignore-files*
           #:*ignore-defs*))

(in-package :lisp-critic-warnings)

(declaim (declaration critique))

(defvar *critic-warnings* t
  "Variable for enabling or disabling Lisp critic warnings.")

(defvar *ignore-packages* '()
  "The list of packages to ignore when critiquing.")

(defvar *ignore-files* '()
  "The list of files to ignore when critiquing.")

(defvar *ignore-defs* '()
  "The list of functions to ignore when critiquing.")

(define-condition lisp-critic-style-warning (alexandria:simple-style-warning)
  ())

(defun reformat-critique (critique)
  "Remove the separators from CRITIQUE."
  (with-input-from-string (in critique)
    (let ((lines (uiop/stream:slurp-stream-lines in)))
      (with-output-to-string (s)
        (dolist (line (butlast (rest lines)))
          (write-string line s)
          (terpri s))))))

(defun toggle-critiques (enable? what)
  (etypecase what
    (package (if enable?
                 (alexandria:removef *ignore-packages* (package-name what))
                 (pushnew (package-name what) *ignore-packages*)))
    (pathname (if enable?
                  (alexandria:removef *ignore-files* what )
                  (pushnew what *ignore-files*)))
    (symbol (if enable?
                (alexandria:removef *ignore-defs* what)
                (pushnew what *ignore-defs*)))))

(defun critique-file (file &rest args)
  "Critique definitions found in FILE, using patterns in NAMES."
  (declare (ignore args))
  (let ((names (lisp-critic::get-pattern-names)))
    (with-open-file (in file)
      (let ((eof (list nil)))
        (do ((code (read in nil eof) (read in nil eof)))
            ((eq code eof) (values))
          (cond
            ((and (eql (car code) 'declaim)
                  (eql (cadr code) 'critique))
             ;; a critique declamation
             (destructuring-bind (enable? &optional (what :file)) (cdddr code)
               (toggle-critiques enable?
                                 (ecase what
                                   (:file (pathname file))
                                   (:package *package*)
                                   (t (the symbol what))))))
            (t
             (unless (or (member file *ignore-files*)
                         (member (package-name *package*) *ignore-packages*)
                         (and (eql (car code) 'defun)
                              (member (cadr code) *ignore-defs*)))
               (let ((critique
                       (with-output-to-string (out)
                         (lisp-critic::critique-definition code out names))))
                 (unless (zerop (length critique))
                   (setq critique (reformat-critique critique))
                   ;; TODO: the signaled condition does not contain a source code location.
                   ;; Would that be possible to add?
                   (warn 'lisp-critic-style-warning :format-control critique)))))))))))

(defun maybe-critique-file (file &rest args)
  (when *critic-warnings*
    (apply #'critique-file file args)))

(pushnew 'maybe-critique-file compiler-hooks:*after-compile-file-hooks*)

(provide :lisp-critic-warnings)
