;;; type-annotations --- Support for inline type annotations.
;;
;; Copyright (C) 2023 Mariano Montone. All rights reserved.
;;
;; This work is licensed under the terms of the MIT license.
;; For a copy, see <https://opensource.org/licenses/MIT>.
;;
;; Author: Mariano Montone <marianomontone@gmail.com>
;; Version: 0.1
;;
;;; Commentary:
;;
;; Support for inline type annotations.
;; Versions of CL definitions with support for inline type annotations are provided.
;; Typed DEFUN, DEFVAR, DEFPARAMETER, etc.
;;
;; Type annotations appear after variables and arguments names, and are enclosed between sharp brackets, like: `<type-name>`.
;;
;; Usage:
;;
;; Use the inline type version of the CL equivalent definer, and inline type annotations:
;;
;;     (<t>:defun sum (x <integer> y <integer>) <integer>
;;        (+ x y))
;;
;; Annotated definitions are macro-expanded to top-level type declamations:
;;
;;    (PROGN
;;        (DECLAIM (FTYPE (FUNCTION (INTEGER INTEGER) INTEGER) SUM))
;;        (COMMON-LISP:DEFUN SUM (X Y) (+ X Y)))
;;
;;; Code:

(defpackage :type-annotations
  (:nicknames :<t>)
  (:use :cl)
  (:shadow #:defun #:flet
           #:defvar #:defparameter)
  (:export
   #:defun
   #:flet
   #:defvar
   #:defparameter))

(in-package :type-annotations)

(cl:defun tree-remove-if (predicate tree)
  (if (atom tree)
      (if (funcall predicate tree)
          nil
          tree)
      (let ((car (tree-remove-if predicate (car tree)))
            (cdr (tree-remove-if predicate (cdr tree))))
        (if (null car)
            cdr
            (cons car cdr)))))

(defstruct type-annotation
  type)

(cl:defun remove-type-annotations (annotated-def)
  (destructuring-bind (def name annotated-args &body annotated-body)
      annotated-def
    `(,def ,name ,(tree-remove-if #'type-annotation-p annotated-args)
       ,@(if (type-annotation-p (first annotated-body))
             (rest annotated-body)
             annotated-body))))

(cl:defun parse-type-annotations (list-of-symbols)
  ;; This is the hack for parsing the annotations syntax
  ;; Replace #\< by '#S(type-annotation :type '
  ;; and #\> by #\), that closes the type-annotation expression.
  ;; For instance, '<integer>' is replaced to '#s(type-annotation :type 'integer)'.
  ;; Then READ-FROM-STRING is used to obtain the type-annotation structures.
  (let* ((str (prin1-to-string list-of-symbols))
         (str (ppcre:regex-replace-all "\\<" str "#S(TYPE-ANNOTATIONS::TYPE-ANNOTATION :TYPE ("))
         (str (ppcre:regex-replace-all ">" str "))")))
    (read-from-string str)))

(cl:defun annotate-defun (defun)
  (destructuring-bind (defun name args &body body)
      defun
    (multiple-value-bind (return-type actual-body)
        (extract-return-type body)
      (list* defun name (parse-type-annotations args)
             return-type
             actual-body))))

(cl:defun cl-type (type-annotation)
  (let ((cl-type (type-annotation-type type-annotation)))
    (if (and (listp cl-type) (null (cdr cl-type)))
        (car cl-type)
        cl-type)))

(cl:defun extract-function-types (annotated-defun)
  (let ((status :required)
        (arg-position :arg)
        (required '())
        (required-types '())
        (optional '())
        (optional-types '())
        (key '())
        (key-types '())
        (rest nil)
        (rest-type (make-type-annotation :type 't))
        (allow-other-keys nil))
    (cl:flet ((complete-types ()
                ;; If at type position, then complete the types with T type
                (when (eql arg-position :type)
                  (ecase status
                    (:required (push (make-type-annotation :type 't) required-types))
                    (:optional (push (make-type-annotation :type 't) optional-types))
                    (:rest nil)
                    (:key (push (make-type-annotation :type 't) key-types))
                    (:allow-other-keys nil)))))
      (destructuring-bind (defun name args return-type &body body) annotated-defun
        (declare (ignore defun name body))
        (dolist (arg args) args
          (block next
            (when (eql arg '&optional)
              (complete-types)
              (setf status :optional)
              (setf arg-position :arg)
              (return-from next))
            (when (eql arg '&key)
              (complete-types)
              (setf status :key)
              (setf arg-position :arg)
              (return-from next))
            (when (eql arg '&rest)
              (complete-types)
              (setf status :rest)
              (setf arg-position :arg)
              (return-from next))
            (when (eql arg '&allow-other-keys)
              (complete-types)
              (setf status :allow-other-keys)
              (setf arg-position :arg)
              (setf allow-other-keys t))
            (ecase status
              (:required
               (ecase arg-position
                 (:arg
                  (push arg required)
                  (setf arg-position :type))
                 (:type
                  (if (not (type-annotation-p arg))
                      (progn
                        (push (make-type-annotation :type 't) required-types)
                        (push arg required))
                      (progn
                        (push arg required-types)
                        (setf arg-position :arg))))))
              (:optional
               (tagbody retry
                  (ecase arg-position
                    (:arg
                     (cond
                       ((atom arg)
                        (push arg optional)
                        (setf arg-position :type))
                       ((listp arg)
                        (push (first arg) optional)
                        (let ((type
                                (find-if #'type-annotation-p arg)))
                          (if type
                              (push type optional-types)
                              (push (make-type-annotation :type 't) optional-types))))))
                    (:type
                     (when (not (type-annotation-p arg))
                       (push (make-type-annotation :type 't) optional-types)
                       (setf arg-position :arg)
                       (go retry))
                     (push arg optional-types)
                     (setf arg-position :arg)))))
              (:key
               (tagbody retry
                  (ecase arg-position
                    (:arg
                     (cond
                       ((atom arg)
                        (push arg key))
                       ((listp arg)
                        (push (first arg) key)
                        (let ((type
                                (find-if #'type-annotation-p arg)))
                          (if type
                              (push type key-types)
                              (push (make-type-annotation :type 't) key-types)))))
                     (setf arg-position :type))
                    (:type
                     (when (not (type-annotation-p arg))
                       (push (make-type-annotation :type 't) key-types)
                       (setf arg-position :arg)
                       (go retry))
                     (push arg key-types)
                     (setf arg-position :arg)))))
              (:rest (ecase arg-position
                       (:arg
                        (setf rest arg)
                        (setf arg-position :type))
                       (:type
                        (when (type-annotation-p arg)
                          (setf rest-type arg))
                        (setf arg-position :arg))))
              (:allow-other-keys))))
        (complete-types)
        (values (mapcar #'cons
                        (reverse required)
                        (reverse required-types))
                (mapcar #'cons
                        (reverse optional)
                        (reverse optional-types))
                (when rest (cons rest rest-type))
                (mapcar #'cons
                        (reverse key)
                        (reverse key-types))
                allow-other-keys return-type)))))

(cl:defun make-keyword (symbol)
  (intern (string-upcase (string symbol)) :keyword))

(cl:defun extract-cl-function-type (annotated-defun)
  (multiple-value-bind (required optional rest key allow-other-keys return)
      (extract-function-types annotated-defun)
    `(function (,@(mapcar (alexandria:compose #'cl-type #'cdr) required)
                ,@(when optional
                    (list* '&optional (mapcar (alexandria:compose #'cl-type #'cdr) optional)))
                ,@(when rest
                    (list '&rest (cl-type (cdr rest))))
                ,@(when key
                    (list* '&key (mapcar (lambda (keyarg)
                                           (list (make-keyword (car keyarg))
                                                 (cl-type (cdr keyarg))))
                                         key)))
                ,@(when allow-other-keys
                    (list '&allow-other-keys)))
               ,(cl-type return))))

(cl:defun count-ocurrences (what sequence)
  (loop with count := 0
        for x across sequence
        when (eql x what)
          do (incf count)
        finally (return count)))

(cl:defun extract-return-type (body)
  (let ((first (first body)))
    (if (or (null body)
            (not (symbolp first))
            (member first '(< <=))
            (not (find #\< (symbol-name first))))
        (values (make-type-annotation :type 't) body)
        ;; else, a symbol that starts a type annotation
        (let* ((rest-body body)
               (symbol (pop rest-body))
               (count 0)
               (return-type (list symbol)))
          (incf count (count-ocurrences #\< (symbol-name symbol)))
          (decf count (count-ocurrences #\> (symbol-name symbol)))
          (loop while (and (not (zerop count))
                           rest-body)
                do
                   (setf symbol (pop rest-body))
                   (push symbol return-type)
                   (when (symbolp symbol)
                     (incf count (count-ocurrences #\< (symbol-name symbol)))
                     (decf count (count-ocurrences #\> (symbol-name symbol)))))
          (values (first (parse-type-annotations (reverse return-type)))
                  rest-body)))))

(defmacro defun (name args &body body)
  "Typed annotated DEFUN.

Type annotations can appear after argument names.
Annotations on all required, optional and key arguments are supported.
Also, an optional return type annotation can appear after the function arguments list.

Example:

    (<t>:defun sum (x <integer> y <integer>) <integer>
        (+ x y))

The macro is expanded to a top-level type declamation plus a normal DEFUN:

    (PROGN
         (DECLAIM (FTYPE (FUNCTION (INTEGER INTEGER) INTEGER) SUM))
         (COMMON-LISP:DEFUN SUM (X Y) (+ X Y)))
"
  (let* ((annotated-def (annotate-defun `(defun ,name ,args ,@body)))
         (function-type (extract-cl-function-type annotated-def))
         (untyped-definition (remove-type-annotations annotated-def)))
    `(progn
       (declaim (ftype ,function-type ,name))
       ,(destructuring-bind (defun name args &body body) untyped-definition
          (declare (ignore defun))
          `(cl:defun ,name ,args ,@body)))))

(defmacro defvar (name &rest init)
  "Type annotated DEFVAR.

A type annotation is accepted after the variable name.

Example:

    (<t>:defvar *my-var* <integer> 22)

The macro is expanded to a top-level type declamation plus a normal DEFVAR:

    (PROGN (DECLAIM (TYPE INTEGER *MY-VAR*))
           (COMMON-LISP:DEFVAR *MY-VAR* 22))
"
  (let ((annot-init (parse-type-annotations init)))
    (if (type-annotation-p (car annot-init))
        `(progn
           (declaim (type ,(cl-type (car annot-init)) ,name))
           (cl:defvar ,name ,@(rest annot-init)))
        `(cl:defvar ,name ,@annot-init))))

(defmacro defparameter (name &rest init)
  "Type annotated DEFPARAMETER.

A type annotation is accepted after the variable name.

Example:

    (<t>:defparameter *my-var* <integer> 22)

The macro is expanded to a top-level type declamation plus a normal DEFPARAMETER:

    (PROGN (DECLAIM (TYPE INTEGER *MY-VAR*))
           (COMMON-LISP:DEFPARAMETER *MY-VAR* 22))
"
  (let ((annot-init (parse-type-annotations init)))
    (if (type-annotation-p (car annot-init))
        `(progn
           (declaim (type ,(cl-type (car annot-init)) ,name))
           (cl:defparameter ,name ,@(rest annot-init)))
        `(cl:defparameter ,name ,@annot-init))))

(provide :type-annotations)
