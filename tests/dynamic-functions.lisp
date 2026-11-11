(require :mutils)
(require :dynamic-functions)
(require :fiveam)

(defpackage dynamic-functions-test
  (:use :cl :dynamic-functions :fiveam))

(in-package :dynamic-functions-test)


(defdynfun dynfun-test-1 (x y)
  (+ x y))

(def-test test-1 ()
  (is (= 3 (dynfun-test-1 1 2)))

  (dflet ((dynfun-test-1 (x y)
                         (* x y)))
    (is (= 2 (dynfun-test-1 1 2))))
  (is (= 3 (dynfun-test-1 1 2))))

(def-test dflambda-test ()
  ;; see that dflambda is needed. dynamic scope is lost in the lambda:
  (let (fn)
    (dflet ((dynfun-test-1 (x y)
                           (* x y)))
      (setf fn
            (lambda ()
              (dynfun-test-1 1 2))))
    (is (= 3 (funcall fn))))

  ;; correct version using dflambda:
  (let (fn)
    (dflet ((dynfun-test-1 (x y)
                           (* x y)))
      (setf fn
            (dflambda ()
              (dynfun-test-1 1 2))))
    (is (= 2 (funcall fn)))))

;; convert a normal already defined function to a dynamic function

(defun foo ()
  "foo")

(defun bar ()
  (foo))

(convert-to-dynfun 'foo)

(def-test convert-test ()
  (is (string= "foo" (bar)))
  (dflet ((foo ()
               "bar"))
    (is (string= (bar) "bar")))
  (is (string= "foo" (bar))))
