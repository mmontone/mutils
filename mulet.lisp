;;; mulet --- Let* with destructruing and multiple value bind.

;; Copyright (C) 2025 Mariano Montone. All rights reserved.

;; This work is licensed under the terms of the MIT license.
;; For a copy, see <https://opensource.org/licenses/MIT>.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Version: 0.1

;;; Commentary:
;;
;; Let* with destructruing and multiple value bind.
;;
;; Usage:
;;
;; If more than one variable is used at binding position, then they are bind via MULTIPLE-VALUE-BIND.
;; Example:
;;
;;     (mulet ((res found-p (gethash :foo my-table))) ...)
;;
;; If a list is used at binding position, then DESTRUCTURING-BIND is applied.
;; Example:
;;
;;     (mulet (((x &key z) (list 'x :z 'z))) (list x z))
;;
;;
;;; Code:

(defpackage :mulet
  (:use #:cl)
  (:shadow #:let*)
  (:export #:let*
           #:mulet))

(in-package :mulet)

(cl:defun process-let-binding (binding body)
  "Process MULET BINDING, and return a LET binding plus a new BODY."
  (cond
    ;; normal let binding
    ((and (= (length binding) 2)
          (symbolp (car binding)))
     `((let (,binding) ,@body)))
    ;; multiple-value binding
    ((> (length binding) 2)
     `((multiple-value-bind ,(butlast binding) ,(car (last binding))
         ,@body)))
    ;; destructuring
    ((and (= (length binding) 2)
          (listp (car binding)))
     `((destructuring-bind ,(first binding) ,(second binding)
         ,@body)))))

(defmacro let* (bindings &body body)
  "Upgraded version of CL:LET* that supports destructuring and multiple-value binds.

Usage:

If more than one variable is used at binding position, then they are bind via MULTIPLE-VALUE-BIND.

Example:

    (let* ((res found-p (gethash :foo my-table))) ...)

If a list is used at binding position, then DESTRUCTURING-BIND is applied.
Example:

    (let* (((x &key z) (list 'x :z 'z))) (list x z))
"
  (when (every (cl:lambda (binding)
                 (and (= (length binding) 2)
                      (symbolp (car binding))))
               bindings)
    (return-from let*
      `(cl:let* ,bindings ,@body)))

  (cl:let ((new-body body))
    (cl:dolist (binding (reverse bindings))
      (let ((binding-body
              (process-let-binding binding new-body)))
        (setf new-body binding-body)))
    (car new-body)))

(defmacro mulet (bindings &body body)
  "Upgraded version of CL:LET* that supports destructuring and multiple-value binds.

Usage:

If more than one variable is used at binding position, then they are bind via MULTIPLE-VALUE-BIND.

Example:

    (let* ((res found-p (gethash :foo my-table))) ...)

If a list is used at binding position, then DESTRUCTURING-BIND is applied.
Example:

    (let* (((x &key z) (list 'x :z 'z))) (list x z))
"
  (when (every (cl:lambda (binding)
                 (and (= (length binding) 2)
                      (symbolp (car binding))))
               bindings)
    (return-from mulet
      `(cl:let* ,bindings ,@body)))

  (cl:let ((new-body body))
    (cl:dolist (binding (reverse bindings))
      (let ((binding-body
              (process-let-binding binding new-body)))
        (setf new-body binding-body)))
    (car new-body)))

#+test
(macroexpand-1 '(mulet ((x 34) (y 44)) (cons x y)))

#+test
(macroexpand-1 '(mulet ((res found-p (gethash :key table)))))

#+test
(mulet ((res found-p (gethash :key table))))

#+test
(macroexpand-1 '(mulet (((x y) (list 1 2))) (list x y)))

#+test
(mulet (((x y) (list 1 2))) (list x y))

#+test
(mulet (((x . y) (cons 1 2))) (list x y))

#+test
(mulet ((x y z (values 1 2 3))) (list x y z))
