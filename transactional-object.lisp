(require :closer-mop)

(defpackage :transactional-object
  (:use :cl)
  (:export #:call-with-transaction
           #:*transactions*
           #:transaction
           #:with-transaction
           #:transactional-object
           #:transaction-values
           #:transaction-status
           #:current-transaction
           #:transactional-class
           #:commit
           #:commit-transaction
           #:begin
           #:rollback
           #:rollback-transaction))

(in-package :transactional-object)

(defvar *transactions* nil)
(setf (documentation '*transactions* 'variable) "A list of current transactions")

(defclass transaction ()
  ((values :initform (make-hash-table)
           :accessor transaction-values
           :documentation "A hash table with modified objects as keys and slot values as values.")
   (status :initform :new
           :accessor transaction-status
           :documentation "The transaction status. May be :new, :commited, :failed, etc")))

(defclass transactional-object ()
  ()
  (:documentation "Changes of a transactional object are placed in the current transaction."))

(defun transactional-slot-p (instance slot-name)
  t)

(defun current-transaction ()
  (first *transactions*))

(defun get-transaction-value (transaction object slot-name)
  (let ((slot-value-table (gethash object (transaction-values transaction))))
    (when slot-value-table
      (gethash slot-name slot-value-table))))

(defun set-transaction-value (transaction object slot-name value)
  ;; initialize slot-values table if not there
  (when (not (gethash object (transaction-values transaction)))
    (setf (gethash object (transaction-values transaction))
          (make-hash-table)))
  (let ((slots-value-table (gethash object (transaction-values transaction))))
    (setf (gethash slot-name slots-value-table) value)))

(defun read-transaction-value (object slot-name)
  "Tries to read the value of SLOT-NAME of OBJECT from the current transactions.
Returns the value, and a boolean indicating if the value was found."
  (dolist (transaction *transactions*)
    (multiple-value-bind (value found-p)
        (get-transaction-value transaction object slot-name)
      (when found-p
        (return-from read-transaction-value (values value found-p))))))

;; transactional class

(defclass transactional-class (standard-class)
  ())

(defmethod closer-mop:validate-superclass ((class transactional-class)
                                           (superclass standard-class))
  t)

(defmethod closer-mop:slot-value-using-class ((class transactional-class)
                                              (instance standard-object)
                                              slot-def)
  "Get the slot value from the current transaction."
  (if (and (current-transaction)
           (transactional-slot-p instance (closer-mop:slot-definition-name slot-def)))
      ;; read the value from the current transaction
      (let ((slot-name (closer-mop:slot-definition-name slot-def)))
        (multiple-value-bind (value found-p) (read-transaction-value instance slot-name)
          (if found-p
              value
              ;; if value not found in transactions, return the value of the slot
              (let ((*transactions* nil))
                (call-next-method)))))
      ;; otherwise
      (call-next-method)))

(defmethod (setf closer-mop:slot-value-using-class) (new-value (class transactional-class)
                                                     (instance standard-object)
                                                     slot-def)
  "Set the slot value in the transaction."
  (if (transactional-slot-p instance (closer-mop:slot-definition-name slot-def))
      ;; a transactional slot is trying to be modified
      (let ((transaction (current-transaction)))
        (if transaction
            (let ((slot-name (closer-mop:slot-definition-name slot-def)))
              (set-transaction-value transaction instance slot-name new-value))
            ;; else, there's no transaction
            (call-next-method)))
      ;; else, it is a normal slot, modify it
      (call-next-method)))

(defun pop-transaction (transaction)
  (let ((current-transaction (pop *transactions*)))
    (when (not current-transaction)
      (error "Cannot pop transaction: ~s" transaction))
    (when (eq current-transaction transaction)
      (return-from pop-transaction current-transaction))
    (pop-transaction transaction)))

(defun commit-transaction (transaction)
  ;; ensure TRANSACTION is not in the list of transactions
  (assert (not (member transaction *transactions*)))
  (maphash (lambda (object slots-values)
             (maphash (lambda (slot-name slot-value)
                        (setf (slot-value object slot-name) slot-value))
                      slots-values))
           (transaction-values transaction)))

(defun rollback-transaction (transaction)
  (assert (not (member transaction *transactions*)))
  (setf (transaction-values transaction) (make-hash-table)))

(defun commit ()
  (assert (not (null *transactions*)))
  (let ((transaction (pop *transactions*)))
    (commit-transaction transaction)))

(defun rollback ()
  (assert (not (null *transactions*)))
  (let ((transaction (pop *transactions*)))
    (rollback-transaction transaction)))

(defun begin (&optional (transaction-class 'transaction))
  (push (make-instance transaction-class) *transactions*))

(defun call-with-transaction (func &optional (transaction-class 'transaction))
  (let ((*transactions* (cons (make-instance transaction-class)
                              *transactions*)))
    (funcall func)))

(defmacro with-transaction ((&optional transaction-class) &body body)
  (if transaction-class
      `(call-with-transaction (lambda () ,@body) ',transaction-class)
      `(call-with-transaction (lambda () ,@body))))

;; test

(defclass my-obj ()
  ((x :initarg :x)
   (y :initarg :y))
  (:metaclass transactional-class))

(let ((obj (make-instance 'my-obj :x 1 :y 2)))
  (with-output-to-string (*standard-output*)
    (with-transaction ()
      (setf (slot-value *o1* 'x) 3)
      (write (slot-value *o1* 'x)))
    (write (slot-value *o1* 'x))))

(with-transaction ()
  (setf (slot-value *o1* 'x) 3)
  (print (slot-value *o1* 'x))
  (commit))

(slot-value *o1* 'x)

(with-transaction ()
  (setf (slot-value *o1* 'x) 3)
  (print (slot-value *o1* 'x))
  (rollback)
  (print (slot-value *o1* 'x)))

;; nested transactions

(let ((obj (make-instance 'my-obj :x 1 :y 2)))
  (with-output-to-string (*standard-output*)
    (with-transaction ()
      (setf (slot-value obj 'x) 4)
      (with-transaction ()
        (setf (slot-value obj 'x) 3)
        (write (slot-value obj 'x))
        (rollback))
      (write (slot-value obj 'x))
      (commit))
    (write (slot-value obj 'x))))

;; bind current transaction in callbacks

(let ((obj (make-instance 'my-obj :x 1 :y 2))
      callback
      transactions)
  (begin)
  ;; bind transactions lexically to restore in callbacks
  (let ((transactions *transactions*))
    (setf callback
          (lambda ()
            ;; restore transactions
            (let ((*transactions* transactions))
              (incf (slot-value obj 'x))
              (print (slot-value obj 'x))))))
  (funcall callback)
  ;; simulate other transaction ocurr in the meantime
  (setf *transactions* nil)

  ;; call callbacks with transaction restored
  (funcall callback)
  (funcall callback)
  (slot-value obj 'x))
