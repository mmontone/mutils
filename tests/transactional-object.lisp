(require :mutils)
(require :transactional-object)
(require :fiveam)

(defpackage transactional-object-tests
  (:use :cl :transactional-object :fiveam))

(in-package :transactional-object-tests)

(defclass my-obj ()
  ((x :initarg :x)
   (y :initarg :y))
  (:metaclass transactional-class))

(def-test test-1 ()
  (let ((obj (make-instance 'my-obj :x 1 :y 2)))
    (with-output-to-string (*standard-output*)
      (with-transaction ()
        (setf (slot-value obj 'x) 3)
        (is (= (slot-value obj 'x) 3)))
      (is (= (slot-value obj 'x) 1)))))

(def-test test-2 ()
  (let ((obj (make-instance 'my-obj :x 1 :y 2)))
    (with-transaction ()
      (setf (slot-value obj 'x) 3)
      (is (= (slot-value obj 'x)))
      (commit))
    (is (= (slot-value obj 'x)))))

(def-test test-3 ()
  (let ((obj (make-instance 'my-obj :x 1 :y 2)))
    (with-transaction ()
      (setf (slot-value obj 'x) 3)
      (is (= (slot-value obj 'x) 3))
      (rollback)
      (is  (= (slot-value obj 'x) 1)))))

;; nested transactions

(def-test test-nested ()
  (let ((obj (make-instance 'my-obj :x 1 :y 2)))
    (with-transaction ()
      (setf (slot-value obj 'x) 4)
      (with-transaction ()
        (setf (slot-value obj 'x) 3)
        (is (= (slot-value obj 'x) 3))
        (rollback))
      (is (= (slot-value obj 'x) 4))
      (commit))
    (is (= (slot-value obj 'x) 4))))

;; bind current transaction in callbacks

(def-test test-bind ()
  (let ((obj (make-instance 'my-obj :x 1 :y 2))
        callback
        transactions)
    ;; bind transactions lexically to restore in callbacks
    (let ((transaction (begin))
          (transactions *transactions*))
      (setf callback
            (lambda ()
              ;; restore transactions
              (let ((*transactions* transactions))
                (incf (slot-value obj 'x))
                (slot-value obj 'x))))
      (funcall callback)
      ;; simulate other transaction ocurr in the meantime
      (setf *transactions* nil)

      ;; call callbacks with transaction restored
      (funcall callback)
      (funcall callback)
      (commit-transaction transaction)
      (is (= (slot-value obj 'x) 4)))))
