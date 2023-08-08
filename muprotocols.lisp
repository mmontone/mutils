;;; muprotocols --- An implementation of protocols that plays nicely with Common Lisp type system.

;; Copyright (C) 2023 Mariano Montone. All rights reserved.

;; This work is licensed under the terms of the MIT license.
;; For a copy, see <https://opensource.org/licenses/MIT>.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Version: 0.1

;;; Commentary:
;;
;; An implementation of protocols that plays nicely with Common Lisp type system.
;;
;; A protocol is a nominated set of generic functions that are then implemented by Lisp types.
;;
;; The fact that a type implements a certain protocol is reflected on the Lisp type system, via IMPLEMENTS types.
;;
;; Here is an example usage:
;; Let's define two protocols, one for mutable collections, and another for indexable collections.
;;
;; Elements can be added and removed to/from mutable collections.
;;
;; Protocols are defined via DEFPROTOCOL, that takes a name and a list of definitions, which follow the syntax of GENERIC-FUNCTION definitions:
;;
;;     (defprotocol mutable
;;       (add (thing mutable)
;;            (:documentation "Add THING into MUTABLE."))
;;       (remove* (thing mutable)
;;            (:documentation "Remove THING from MUTABLE.")))
;;
;; Indexable collections can be accessed at an index:
;;
;;    (defprotocol indexable
;;       (at (index indexable)
;;          (:documentation "Get element of INDEXABLE at INDEX.")))
;;
;; Protocols are implemented by a type using IMPLEMENT-PROTOCOL:
;;
;; Our list collections are both mutable and indexable:
;;
;;     (defstruct list-collection
;;       list)
;;
;;     (implement-protocol mutable list-collection
;;        (add (thing (coll list-collection))
;;          (push thing (list-collection-list coll)))
;;        (remove* (thing (coll list-collection))
;;           (setf (list-collection-list coll)
;;                 (remove thing (list-collection-list coll)))))
;;
;;     (implement-protocol indexable list-collection
;;        (at ((index integer) (coll list-collection))
;;           (nth index (list-collection-list coll))))
;;
;; Our set collections are mutable, but not indexable:
;;
;;     (defstruct set-collection
;;        set)
;;
;;     (implement-protocol mutable set-collection
;;        (add (thing (coll set-collection))
;;           (pushnew thing (set-collection-set coll)))
;;        (remove* (thing (coll set-collection))
;;           (setf (set-collection-set coll)
;;                 (remove thing (set-collection-set coll)))))
;;
;; The implementation of a protocol actually defines CLOS methods that specialize on the type involved:
;;
;;     (defparameter *list* (make-list-collection))
;;     (defparameter *set* (make-set-collection))
;;
;;     (add 1 *list*)
;;     (add 2 *list*)
;;     (at 0 *list*) => 1
;;
;;     (add 1 *set*)
;;     (add 2 *set*)
;;     (add 2 *set*)
;;     (at 0 *set*) => error. Not indexable.
;;
;; The implementation of a protocol extends the Lisp type system.
;; Types that implement a protocol satisfy the type `(implements &rest protocols)`.
;;
;; We can check that for our example:
;;
;;     (typep (make-list-collection) '(implements mutable)) => t
;;     (typep (make-list-collection) '(implements mutable indexable)) => t
;;     (typep (make-set-collection) '(implements mutable)) => t
;;     (typep (make-set-collection) '(implements indexable)) => nil
;;
;; Now we can use Common Lisp type system to restrict the types of inputs to functions based on protocols either:
;;
;; 1) Declaring function types at top-level:
;;
;;     ```
;;     (declaim (ftype (function ((implements mutable indexable)) t)
;;                     mutate-indexable-collection))
;;
;;     (defun mutate-indexable-collection (coll)
;;        (add "foo" coll)
;;        (at 0 coll))
;;     ```
;; If we try to compile a function call to MUTATE-INDEXABLE-COLLECTION with something that is not both mutable and indexable:
;;
;;     (mutate-indexable-collection (make-set-collection))
;;
;; we get a compile-time error:
;;
;;     The value #S(set-collection :set nil) is not of type
;;      (and
;;       (satisfies muprotocols-example::implements-mutable-protocol-p)
;;       (satisfies muprotocols-example::implements-indexable-protocol-p))
;;
;; 2) Using local function declarations:
;;
;;     ```
;;     (defun mutate-indexable-collection-2 (coll)
;;         (declare (type (implements mutable indexable) coll))
;;         (add "foo" coll)
;;         (at 0 coll))
;;     ```
;;
;; 3) Don't use the type system. Use CHECK-IMPLEMENTS:
;;
;;     ```
;;     (defun mutate-indexable-collection-3 (coll)
;;         (check-implements coll mutable indexable)
;;         (add "foo" coll)
;;         (at 0 coll))
;;     ```
;;
;;; Code:

(defpackage :muprotocols
  (:use :cl)
  (:export
   #:defprotocol
   #:implement-protocol
   #:check-implements
   #:implements
   #:implements-protocol-p)
  (:documentation "Protocols that play nicely with Common Lisp type system."))

(in-package :muprotocols)

(defvar *protocols* (make-hash-table)
  "The defined protocols.")

(defstruct protocol
  "The protocol structure."
  name
  definitions
  documentation)

(declaim (ftype (function (symbol &optional boolean) (or protocol null))
                find-protocol))
(defun find-protocol (name &optional (error-p t))
  "Find a PROTOCOL by NAME."
  (or (gethash name *protocols*)
      (when error-p
        (error "Protocol not defined: ~s" name))))

(defgeneric implements-protocol-p (object protocol)
  (:documentation "Tests if OBJECT implements PROTOCOL.")
  (:method (object protocol)
    (find-protocol protocol)
    nil))

(defun implements-protocols-p (object &rest protocols)
  "Tests if OBJECT implements PROTOCOLS."
  (every (lambda (protocol)
           (implements-protocol-p object protocol))
         protocols))

(defun protocol-satisfies-predicate-name (protocol-name)
  "Name for the SATISFY predicate."
  (intern
   (format nil "IMPLEMENTS-~a-PROTOCOL-P"
           (string-upcase (string protocol-name)))))

(defmacro defprotocol (name &body definitions)
  "Define a protocol.

Protocols have a NAME followed by generic function DEFINITIONS.

Syntax:

    defprotocol ::= (name [documentation] definitions*)
    definitions ::= definition*
    definition ::= (function-name gf-lambda-list)

Example:

    (defprotocol indexable
       \"Protocol for indexable objects\"
       (get-at (index indexable)
         (:documentation \"Get element at INDEX from INDEXABLE.\"))
       (set-at (value index indexable)
         (:documentation \"Set element at INDEX from INDEXABLE.\")))

Definitions follow the syntax of DEFGENERIC.
It is required that the name of the protocol (`indexable` in the example)
appears in all definitions, in at least one of the arguments, at the positions where the generic functions are passed instances of objects that implement the protocol.

Protocols are implemented by types using IMPLEMENT-PROTOCOL."
  (let ((documentation (when (stringp (first definitions))
                         (first definitions)))
        (definitions (if (stringp (first definitions))
                         (rest definitions)
                         definitions)))
    `(progn
       (setf (gethash ',name *protocols*)
             (make-protocol :name ',name
                            :definitions ',definitions
                            :documentation ,documentation))
       (defun ,(protocol-satisfies-predicate-name name) (object)
         (implements-protocol-p object ',name))
       ,@(loop for def in definitions
               for (def-name def-args &rest def-options) := def
               do (unless (member name def-args)
                    (error "An argument named: ~a should appear in the arguments of definition: ~a of protocol: ~a"
                           name def-name name))
               collect `(defgeneric ,@def))
       ',name)))

(declaim (ftype (function (protocol symbol list) t)
                check-protocol-implementation))
(defun check-protocol-implementation (protocol type implementations)
  "Check that the protocol implementation is valid."
  (let ((missing-implementations
          (set-difference
           (mapcar #'car (protocol-definitions protocol))
           (mapcar #'car implementations)))
        (not-belonging-implementations
          (set-difference
           (mapcar #'car implementations)
           (mapcar #'car (protocol-definitions protocol)))))
    (when missing-implementations
      (error "Missing implementations for ~a for protocol ~a: ~a"
             type (protocol-name protocol) missing-implementations))
    (when not-belonging-implementations
      (error "Not part of the protocol ~a: ~a"
             (protocol-name protocol)
             not-belonging-implementations))
    ;; Check that implementation lambda lists match
    ;; the lambda-lists of the protocol definitions.
    (dolist (implementation implementations)
      (destructuring-bind (name args &body body) implementation
        (declare (ignore body))
        (let ((definition (find name (protocol-definitions protocol)
                                :key #'car)))
          (loop for def-arg in (second definition)
                for impl-arg in args
                do
                   (cond
                     ((and (member def-arg lambda-list-keywords)
                           (not (eql def-arg impl-arg)))
                      (error "~a expected in ~a in place of ~a"
                             def-arg name impl-arg))
                     ((and (member impl-arg lambda-list-keywords)
                           (not (eql def-arg impl-arg)))
                      (error "~a expected in ~a in place of ~a"
                             def-arg name impl-arg))
                     ((and (eql def-arg (protocol-name protocol))
                           (not (and (listp impl-arg)
                                     (eql (second impl-arg) type))))
                      (error "An argument specializing: ~a is expected in: ~a in place of: ~a"
                             type name impl-arg)))))))
    t))

(defmacro implement-protocol (name type &body implementations)
  "Implement an already defined via DEFPROTOCOL.
TYPE is the name of the type that implements the PROTOCOL.

Syntax:

    defprotocol ::= (name type implementation*)
    implementation := (name specialized-lambda-list [{declaration}* | documentation] {form}*)

Implementations follow the syntax of DEFMETHOD. It is required that the TYPE specializer appears in the same position as in the protocol definition.

Example:

    (implement-protocol indexable array
        (get-at ((index integer) (arr array))
            (aref arr index))
        (set-at (value (index integer) (arr array))
            (setf (aref arr index) value)))
"
  (let ((protocol (find-protocol name)))
    (check-protocol-implementation protocol type implementations)
    `(progn
       (defmethod implements-protocol-p ((object ,type) (protocol (eql ',name)))
         t)
       ,@(loop for implementation in implementations
               collect
               `(defmethod ,@implementation))
       ',name)))

(deftype implements (&rest protocols)
  "Type whose instances are implementors of PROTOCOLS."
  (if (null (cdr protocols))
      `(satisfies ,(protocol-satisfies-predicate-name (first protocols)))
      `(and ,@(mapcar (lambda (protocol)
                        `(satisfies ,(protocol-satisfies-predicate-name protocol)))
                      protocols))))

(defmacro check-implements (object &rest protocols)
  "Check that OBJECT implements PROTOCOLS.
An ERROR is signaled if not."
  `(unless (implements-protocols-p ,object ,@(mapcar (lambda (x) `(quote ,x)) protocols))
     (error "~s does not implement protocols: ~{~a~^, ~}" ,object ',protocols)))

(provide :muprotocols)
