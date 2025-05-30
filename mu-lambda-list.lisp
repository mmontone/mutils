;;; mu-lambda-list --- Lambda with destructuring and syntax for ignorable arguments.
;;
;;
;; Version: 0.1
;;
;;; Commentary:
;;
;; Lambda with destructuring and syntax for ignorable arguments.
;;
;; Usage:
;;
;; In your defpackage add a (:shadowing-import-from #:mulambda #:defun #:lambda #:destructuring-bind)

;; TODO: potential idea. Add support for &ignore and &ignorable in lambda-lists
;;

(defpackage :mu-lambda-list
  (:use #:cl)
  (:shadow #:lambda #:destructuring-bind #:defun)
  (:export #:lambda #:destructuring-bind #:defun))

(in-package :mu-lambda-list)

(defmacro destructuring-bind (lambda-list expression &body body)
  (let ((ignore-args (remove-if (cl:lambda (arg)
                                  (char/= (aref (symbol-name arg) 0)
                                          #\_))
                                lambda-list)))
    `(cl:destructuring-bind ,lambda-list ,expression
       ,@(when ignore-args
           `((declare (ignore ,@ignore-args))))
       ,@body)))

(cl:defun process-lambda-list (lambda-list body)
  (let ((ignore-args (remove-if (cl:lambda (arg)
                                  (or (consp arg)
                                      (char/= (aref (symbol-name arg) 0)
                                              #\_)))
                                lambda-list))
        (new-body body)
        (new-args nil))
    (dolist (arg lambda-list)
      (if (consp arg) ;; destructure
          (let ((new-arg (gensym)))
            (setf new-body `((destructuring-bind ,arg ,new-arg
                               ,@new-body)))
            (push new-arg new-args))
          (push arg new-args)))
    (when ignore-args
      (setf new-body
            (list* `(declare (ignore ,@ignore-args))
                   new-body)))
    (values (reverse new-args) new-body)))

(defmacro lambda (lambda-list &body body)
  (multiple-value-bind (new-args new-body)
      (process-lambda-list lambda-list body)
    `(cl:lambda ,new-args ,@new-body)))

(defmacro defun (name lambda-list &body body)
  (multiple-value-bind (new-args new-body)
      (process-lambda-list lambda-list body)
    `(cl:defun ,name ,new-args ,@new-body)))

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
