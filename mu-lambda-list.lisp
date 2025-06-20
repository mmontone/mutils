;;; mu-lambda-list --- Lambda lists with destructuring and ignorable arguments.

;; Copyright (C) 2025 Mariano Montone. All rights reserved.

;; This work is licensed under the terms of the MIT license.
;; For a copy, see <https://opensource.org/licenses/MIT>.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Version: 0.1

;;; Commentary:
;;
;; Lambda lists with destructuring and ignorable arguments.
;;
;; Usage:
;;
;; In your defpackage add a #:shadowing-import-from #:mu-lambda-list and the list of exported symbols you want to import.
;; Like: (:shadowing-import-from #:mu-lambda-list #:defun #:lambda #:destructuring-bind #:multiple-value-bind).
;; Or use MU-LAMBDA-LIST:DEFPACKAGE to define your package.
;;
;; Arguments in lambda-lists that start with _ character are declared ignored.
;; Example:
;;
;;    (lambda (_x) ...)
;;
;; Destructuring is supported in positional arguments in lambda-lists.
;; Example:
;;
;;     (funcall (lambda ((x . y))
;;                  (list x y))
;;             (cons 1 2))
;;
;; TODO: potential idea: Add support for &ignore and &ignorable in lambda-lists
;;
;;; Code:

(defpackage :mu-lambda-list
  (:use #:cl)
  (:shadow #:lambda
           #:destructuring-bind
           #:defun
           #:multiple-value-bind
           #:defpackage
           #:dolist)
  (:export #:lambda
           #:destructuring-bind
           #:defun
           #:multiple-value-bind
           #:defpackage
           #:dolist))

(in-package :mu-lambda-list)

(defmacro defpackage (&rest options)
  `(cl:defpackage ,@options
     (:shadowing-import-from
      #:mu-lambda-list
      #:defun
      #:lambda
      #:destructuring-bind
      #:multiple-value-bind
      #:dolist)))

(defmacro destructuring-bind (lambda-list expression &body body)
  (let ((ignore-args
          (remove-if-not (cl:lambda (arg)
                           (and (symbolp arg)
                                (char= (aref (symbol-name arg) 0)
                                       #\_)))
                         (cond
                           ((listp (cdr lambda-list))
                            lambda-list)
                           ((consp lambda-list)
                            (list (car lambda-list) (cdr lambda-list)))
                           (t (error "Don't know how to process destructuring lambda-list: ~s" lambda-list))))))
    `(cl:destructuring-bind ,lambda-list ,expression
       ,@(when ignore-args
           `((declare (ignore ,@ignore-args))))
       ,@body)))

(defmacro multiple-value-bind (lambda-list expression &body body)
  "Upgraded version of CL:MULTIPLE-VALUE-BIND that supports ignorable arguments."
  (let ((ignore (gensym "IGNORE")))
    `(cl:multiple-value-call (lambda (&optional ,@lambda-list &rest ,ignore)
                               (declare (ignore ,ignore))
                               ,@body)
       ,expression)))

(cl:defun process-binding (binding body)
  (cond
    ;; normal binding
    ((and (symbolp (car binding))
          (char/= (aref (symbol-name (car binding)) 0)
                  #\_))
     (values binding body))
    ;; ignore binding
    ((and (symbolp (car binding))
          (char= (aref (symbol-name (car binding)) 0)
                 #\_))
     (values binding (list* `(declare (ignore ,(car binding)))
                            body)))
    ;; destructuring
    ((listp (car binding))
     (values nil `(destructuring-bind ,(first binding) ,(second binding)
                    ,@body)))
    (t (error "Invalid binding: ~s" binding))))

;; TODO: we only support destructuring in positional arguments
(cl:defun process-lambda-list (lambda-list body)
  (let ((ignore-args (remove-if (cl:lambda (arg)
                                  (or (consp arg)
                                      (char/= (aref (symbol-name arg) 0)
                                              #\_)))
                                lambda-list))
        (new-body body)
        (new-args nil)
        (in-required-args t))
    (cl:dolist (arg lambda-list)
      (cond
        ((and (symbolp arg)
              (member arg '(&optional &key &aux &allow-other-keys)))
         (setf in-required-args nil)
         (push arg new-args))
        ((and in-required-args (consp arg)) ;; destructure
         (let ((new-arg (gensym)))
           (setf new-body `((destructuring-bind ,arg ,new-arg
                              ,@new-body)))
           (push new-arg new-args)))
        (t (push arg new-args))))
    (when ignore-args
      (setf new-body
            (list* `(declare (ignore ,@ignore-args))
                   new-body)))
    (values (reverse new-args) new-body)))

(defmacro lambda (lambda-list &body body)
  (cl:multiple-value-bind (new-args new-body)
      (process-lambda-list lambda-list body)
    `(cl:lambda ,new-args ,@new-body)))

(defmacro defun (name lambda-list &body body)
  (cl:multiple-value-bind (new-args new-body)
      (process-lambda-list lambda-list body)
    `(cl:defun ,name ,new-args ,@new-body)))

(defmacro dolist ((var list &optional result) &body body)
  (cond
    ((symbolp var)
     `(cl:dolist (,var ,list ,result)
        ,@body))
    ((listp var)
     (let ((new-var (gensym)))
       `(cl:dolist (,new-var ,list ,result)
          (destructuring-bind ,var ,new-var
            ,@body))))))

#+test
(macroexpand-1
 '(lambda (_x)
   (print "lala")))

#+test
(macroexpand-1
 '(lambda (x)
   (print "lala")))

#+test
(macroexpand-1
 '(lambda ((x &optional y))
   (print x) (print y)))

#+test
(macroexpand
 '(lambda (_x (_ &key y))
   (print y)))

#+test
(funcall
 (lambda ((x y))
   (list y x))
 (list 1 2))

#+test
(funcall
 (lambda ((_ y))
   (list y))
 (list 1 2))

#+test
(dolist ((x . _) '((a . 1) (b . 2)))
  (print x))
