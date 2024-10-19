;;; compiler-hooks --- Provides hooks for Common Lisp compilation api.

;; Copyright (C) 2023 Mariano Montone. All rights reserved.

;; This work is licensed under the terms of the MIT license.  
;; For a copy, see <https://opensource.org/licenses/MIT>.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Version: 0.1
;; Requires: alexandria, cl-package-locks

;;; Commentary:

;; Provides hooks for Common Lisp compilation api.

;;; Code:

(require :cl-package-locks)

(defpackage :compiler-hooks
  (:use :cl)
  (:export
   #:*compiler-hooks-enabled*
   #:*before-compile-hooks*
   #:*after-compile-hooks*
   #:*before-compile-file-hooks*
   #:*after-compile-file-hooks*))

(in-package :compiler-hooks)

(defvar *compiler-hooks-enabled* nil
  "Toggle this variable for enabling or disabling compiler hooks.")

(defvar *after-compile-hooks* nil
  "List of function-designators that are called after COMPILE.")
(defvar *before-compile-hooks* nil
  "List of function-designators that are called before COMPILE.")

(defvar *before-compile-file-hooks* nil
  "List of function-designators that are called before COMPILE-FILE.")
(defvar *after-compile-file-hooks* nil
  "List of function-designators that are called after COMPILE-FILE.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl-package-locks:without-package-locks
    (let ((compile (fdefinition 'compile)))
      (flet ((compile-with-hooks (name &optional definition)
               (if *compiler-hooks-enabled*
                   (progn
                     (dolist (hook *before-compile-hooks*)
                       (funcall hook name definition))
                     (prog1
                         (if definition
                             (funcall compile name definition)
                             (funcall compile name))
                       (dolist (hook *after-compile-hooks*)
                         (funcall hook name definition))))
                   (if definition
                             (funcall compile name definition)
                             (funcall compile name)))))
        (setf (fdefinition 'compile) #'compile-with-hooks)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl-package-locks:without-package-locks
    (let ((compile-file (fdefinition 'compile-file)))
      (flet ((compile-file-with-hooks (input-file &rest args &key output-file verbose
                                                               print external-format)
               (if *compiler-hooks-enabled*
                   (progn
                     (dolist (hook *before-compile-file-hooks*)
                       (apply hook input-file args))
                     (multiple-value-prog1
                         (apply compile-file input-file args)
                       (dolist (hook *after-compile-file-hooks*)
                         (apply hook input-file args))))
                   (apply compile-file input-file args))))
        (setf (fdefinition 'compile-file) #'compile-file-with-hooks)))))

(provide :compiler-hooks)
