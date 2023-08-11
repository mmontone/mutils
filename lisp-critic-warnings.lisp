;;; lisp-critic-warnings --- Signal compiler warnings with lisp-critic critiques.

;; Copyright (C) 2023 Mariano Montone. All rights reserved.

;; This work is licensed under the terms of the MIT license.  
;; For a copy, see <https://opensource.org/licenses/MIT>.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Version: 0.1
;; Requires: lisp-critic, compiler-hooks

;;; Commentary:

;; Signal compiler warnings with lisp-critic critiques.
;;
;; After the module is loaded, LISP-CRITIC warnings with critiques are signaled on COMPILE-FILE calls.
;;
;;; Code:

(require :lisp-critic)
(require :compiler-hooks)

(defpackage :lisp-critic-warnings
  (:use :cl)
  (:export #:*critic-warnings*))

(in-package :lisp-critic-warnings)

(defvar *critic-warnings* t
  "Variable for enabling or disabling Lisp critic warnings.")

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

(defun critique-file (file &rest args)
  "Critique definitions found in FILE, using patterns in NAMES."
  (declare (ignore args))
  (let ((names (lisp-critic::get-pattern-names)))
    (with-open-file (in file)
      (let ((eof (list nil)))
        (do ((code (read in nil eof) (read in nil eof)))
            ((eq code eof) (values))
          (let ((critique
                  (with-output-to-string (out)
                    (lisp-critic::critique-definition code out names))))
            (unless (zerop (length critique))
              (setq critique (reformat-critique critique))
              ;; TODO: the signaled condition does not contain a source code location.
              ;; Would that be possible to add?
              (warn 'lisp-critic-style-warning :format-control critique))))))))

(defun maybe-critique-file (file &rest args)
  (when *critic-warnings*
    (apply #'critique-file file args)))

(pushnew 'maybe-critique-file compiler-hooks:*after-compile-file-hooks*)

(provide :lisp-critic-warnings)
