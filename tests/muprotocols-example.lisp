(defpackage :muprotocols-example
  (:use :cl :muprotocols))

(in-package :muprotocols-example)

(defprotocol mutable
  (add (thing mutable)
       (:documentation "Add THING into MUTABLE."))
  (remove* (thing mutable)
        (:documentation "Remove THING from MUTABLE.")))

(defprotocol indexable
  (at (index indexable)
      (:documentation "Get element of INDEXABLE at INDEX.")))

(typep (list 1 2 3) '(implements mutable))

(defstruct list-collection
  list)

(defstruct set-collection
  set)

(typep (make-list-collection) '(implements mutable))

(implement-protocol mutable list-collection
  (add (thing (coll list-collection))
         (push thing (list-collection-list coll)))
  (remove* (thing (coll list-collection))
           (setf (list-collection-list coll)
                 (remove thing (list-collection-list coll)))))

(typep (make-list-collection) '(implements mutable))
  
(typep (make-list-collection) '(implements mutable indexable))

(implement-protocol indexable list-collection
  (at ((index integer) (coll list-collection))
      (nth index (list-collection-list coll))))
  
(typep (make-list-container) '(implements queue indexable))

(implement-protocol mutable set-collection
  (add (thing (coll set-collection))
       (pushnew thing (set-collection-set coll)))
  (remove* (thing (coll set-collection))
           (setf (set-collection-set coll)
                 (remove thing (set-collection-set coll)))))

(typep (make-set-collection) '(implements mutable))

(typep (make-set-collection) '(implements indexable))

(declaim (ftype (function ((implements mutable indexable)) t)
                mutate-indexable-collection))

(defun mutate-indexable-collection (coll)
  (add "foo" coll)
  (at 0 coll))

(mutate-indexable-collection (make-list-collection))

(mutate-indexable-collection (make-set-collection))

(defun mutate-indexable-collection-2 (coll)
  (declare (type (implements mutable indexable) coll))
  (add "foo" coll)
  (at 0 coll))

(mutate-indexable-collection-2 (make-list-collection))

(mutate-indexable-collection-2 (make-set-collection))

(defun mutate-indexable-collection-3 (coll)
  (check-implements coll mutable indexable)
  (add "foo" coll)
  (at 0 coll))

(mutate-indexable-collection-3 (make-list-collection))

(mutate-indexable-collection-3 (make-set-collection))

(let ((list (make-list-collection))
      (set (make-set-collection)))
  (add 1 list)
  (add 2 set)
  (at 0 set))
