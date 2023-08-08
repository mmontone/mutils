;;; muprotocols --- An implementation of protocols that plays nicely with Common Lisp type system.

;; Copyright (C) 2023 Mariano Montone. All rights reserved.

;; This work is licensed under the terms of the MIT license.
;; For a copy, see <https://opensource.org/licenses/MIT>.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Version: 0.1

;;; Commentary:
;;
;; An implementation of protocols for Common Lisp that plays nicely with Common Lisp type system.
;;
;; This is work in progress at the moment.
;;
;;; Code:

(defpackage :muprotocols
  (:use :cl)
  (:export
   #:defprotocol
   #:implement-protocol
   #:check-implements
   #:implements
   #:implements-protocol-p))

(in-package :muprotocols)

(defvar *protocols* (make-hash-table))

(defstruct protocol
  name
  definitions
  documentation)

(declaim (ftype (function (symbol &optional boolean) (or protocol null))
                find-protocol))
(defun find-protocol (name &optional (error-p t))
  (or (gethash name *protocols*)
      (when error-p
        (error "Protocol not defined: ~s" name))))

(defgeneric implements-protocol-p (object protocol)
  (:method (object protocol)
    (find-protocol protocol)
    nil))

(defun implements-protocols-p (object &rest protocols)
  (every (lambda (protocol)
           (implements-protocol-p object protocol))
         protocols))

(defun protocol-satisfies-name (protocol-name)
  (intern
   (format nil "IMPLEMENTS-~a-PROTOCOL-P"
           (string-upcase (string protocol-name)))))

(defmacro defprotocol (name &body definitions)
  `(progn
     (setf (gethash ',name *protocols*)
           (make-protocol :name ',name
                          :definitions ',definitions
                          :documentation "TODO"))
     (defun ,(protocol-satisfies-name name) (object)
       (implements-protocol-p object ',name))
     ,@(loop for def in definitions
             collect `(defgeneric ,@def))
     ',name))

(declaim (ftype (function (protocol symbol list) t)
                check-protocol-implementation))
(defun check-protocol-implementation (protocol type implementations)
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
    ;; Check that TYPE appears as an specializer
    ;; in the implementation lambda-list
    ;; FIXME: do this in a precise way (perhaps parse the method lambda-list),
    ;; and ensure that appears as a required parameter specializer.
    (dolist (implementation implementations)
      (destructuring-bind (name args &body body) implementation
        (declare (ignore body))
        (unless (member type (mapcar (lambda (arg)
                                       (when (listp arg)
                                         (cadr arg)))
                                     args))
          (error "~a should appear as specializer in lambda-list of definition: ~a"
                 type name))))
    t))

(defmacro implement-protocol (name type &body implementations)
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
  (if (= (length protocols) 1)
      `(satisfies ,(protocol-satisfies-name (first protocols)))
      `(and ,@(mapcar (lambda (protocol)
                        `(satisfies ,(protocol-satisfies-name protocol)))
                      protocols))))

(declaim (ftype (function (t symbol) t)
                check-implements))
(defmacro check-implements (object &rest protocols)
  `(unless (implements-protocols-p ,object ,@(mapcar (lambda (x) `(quote ,x)) protocols))
     (error "~s does not implement protocols: ~{~a~^, ~}" ,object ',protocols)))

(provide :muprotocols)
