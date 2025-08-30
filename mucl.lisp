;;; mucl --- Upgraded versions of CL definitions with destructuring and ignorable arguments at binding position.

;; Copyright (C) 2025 Mariano Montone. All rights reserved.

;; This work is licensed under the terms of the MIT license.
;; For a copy, see <https://opensource.org/licenses/MIT>.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Version: 0.1

;;; Commentary:
;;
;; Upgraded versions of CL definitions with destructuring and ignorable arguments at binding position.
;;
;; Usage:
;;
;; In your defpackage add a #:shadowing-import-from #:mucl and the list of exported symbols you want to import.
;; Like: (:shadowing-import-from #:mucl #:defun #:lambda #:destructuring-bind #:multiple-value-bind).
;; Or use MUCL:DEFPACKAGE to define your package.
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
;;; Code:

(require :access)

(defpackage :mucl
  (:use #:cl)
  (:shadow #:lambda
           #:destructuring-bind
           #:defun
           #:multiple-value-bind
           #:defpackage
           #:dolist
           #:let*
           #:with-accessors
           #:defclass)
  (:export #:lambda
           #:destructuring-bind
           #:defun
           #:multiple-value-bind
           #:defpackage
           #:dolist
           #:let*
           #:with-accessors
           #:defclass
           #:self))

(in-package :mucl)

(defmacro defpackage (&rest options)
  `(cl:defpackage ,@options
     (:shadowing-import-from
      #:mucl
      #:defun
      #:lambda
      #:destructuring-bind
      #:multiple-value-bind
      #:dolist
      #:let*
      #:with-accessors
      #:defclass)))

(defvar *destructurers*
  '((:accessors . destructuring-bind-accessors)
    (:slots . destructuring-bind-slots)
    (:values . destructuring-bind-values)))

(cl:defun destructuring-bind-accessors (binding expression body)
  `(with-accessors ,(cdr binding) ,expression
     ,@body))

(cl:defun destructuring-bind-slots (binding expression body)
  `(with-slots ,(cdr binding) ,expression
     ,@body))

(defmacro destructuring-bind (lambda-list expression &body body)
  "Upgraded version of CL:DESTRUCTURING-BIND that supports ignorable arguments."
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
    (if (keywordp (car lambda-list))
        ;; special destructurer
        (let ((destructurer (cdr (assoc (car lambda-list) *destructurers*))))
          (when (not destructurer)
            (error "Unknown destructurer: ~s" (car lambda-list)))
          (funcall destructurer lambda-list expression body))
        `(cl:destructuring-bind ,lambda-list ,expression
           ,@(when ignore-args
               `((declare (ignore ,@ignore-args))))
           ,@body))))

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
  "Upgraded version of CL:LAMBDA that supports ignorable arguments and destructuring in its lambda-list."
  (cl:multiple-value-bind (new-args new-body)
      (process-lambda-list lambda-list body)
    `(cl:lambda ,new-args ,@new-body)))

(defmacro defun (name lambda-list &body body)
  "Upgraded version of CL:DEFUN that supports ignorable arguments and destructuring in its lambda-list."
  (cl:multiple-value-bind (new-args new-body)
      (process-lambda-list lambda-list body)
    `(cl:defun ,name ,new-args ,@new-body)))

(defmacro dolist ((var list &optional result) &body body)
  "Upgraded version of CL:DOLIST that supports destructuring at variable binding position.

Example:

    (dolist ((x . y) my-list-of-conses) ...)"
  (cond
    ((symbolp var)
     `(cl:dolist (,var ,list ,result)
        ,@body))
    ((listp var)
     (let ((new-var (gensym)))
       `(cl:dolist (,new-var ,list ,result)
          (destructuring-bind ,var ,new-var
            ,@body))))))

;; TODO: allow multiple-value binding + destructruing at the same time
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

(defmacro with-accessors (bindings instance &body body)
  "Upgraded version of CL:WITH-ACCESSORS that supports accessor symbol in bindings.

For example:

    (with-accessors (my-accessor) my-object ...)

expands to:

    (cl:with-accessors ((my-accessor my-accessor)) my-object ...)"
  `(cl:with-accessors ,(loop for binding in bindings
                             collect (if (symbolp binding)
                                         (list binding binding)
                                         binding))
       ,instance
     ,@body))

(defun map-plist (func plist)
  (let ((new-plist ()))
    (alexandria:doplist (key val plist)
      (let ((res (multiple-value-list (funcall func key val))))
        (when (keywordp (first res))
          (push (first res) new-plist)
          (push (second res) new-plist))))
    (nreverse new-plist)))

#+test
(map-plist (lambda (key val)
             (when (not (eq key :foo))
               (values key val)))
           '(:bar 22 :foo 33 :baz 22))

(defun parse-method-spec (spec)
  (if (keywordp (second spec))
      (destructuring-bind (name qualifier args &body body) spec
        (values name qualifier args body))
      (destructuring-bind (name args &body body) spec
        (values name nil args body))))

(defmacro defclass (name direct-superclasses direct-slots &rest options)
  (let ((methods (list))
        (exports (list))
        defclass-slots
        (use-dot-syntax (cadr (find :dot-syntax options :key #'car))))
    (flet ((process-slot-def (slot-def)
             (if (symbolp slot-def)
                 slot-def
                 (destructuring-bind (slot-name &rest slot-options) slot-def
                   (flet ((export-slot (what)
                            (ecase what
                              (:accessor (pushnew (getf slot-options :accessor) exports))
                              (:slot (pushnew slot-name exports))
                              (:reader (pushnew (getf slot-options :reader) exports))
                              (:writer (pushnew (getf slot-options :writer) exports)))))
                     `(,slot-name ,@(map-plist (lambda (key val)
                                                 (case key
                                                   (:export
                                                    (mapc #'export-slot
                                                          (if (listp val)
                                                              val (list val)))
                                                    nil)
                                                   (:required
                                                    (cond
                                                      ((stringp val)
                                                       (values :initform `(error ,val)))
                                                      (t (when val
                                                           (values :initform `(error ,(format nil "~a is required" slot-name)))))))
                                                   (t
                                                    (values key val))))
                                               slot-options)))))))
      (setf defclass-slots (mapcar #'process-slot-def direct-slots)))

    (labels ((build-method (spec)
               (multiple-value-bind (method-name qualifier args body)
                   (parse-method-spec spec)
                 `(defmethod ,method-name
                      ,@(when qualifier (list qualifier))
                    ((self ,name) ,@args)
                    ,@(if use-dot-syntax
                          `((access:with-dot ()
                              ,@body))
                          body))))
             (process-class-option (option)
               (case (car option)
                 (:method
                     (push (build-method (cdr option)) methods))
                 (:export
                  (labels ((process-class-export (export)
                             (ecase export
                               (:class-name
                                (pushnew name exports))
                               (:slots
                                (dolist (slot direct-slots)
                                  (unless (member :export slot)
                                    (pushnew (car slot) exports))))
                               (:accessors
                                (dolist (slot direct-slots)
                                  (unless (member :export slot)
                                    (when (member :accessor slot)
                                      (pushnew (getf (cdr slot) :accessor) exports)))))
                               ((:all t)
                                (process-class-export :slots)
                                (process-class-export :accessors)
                                (process-class-export :class-name)))))
                    (mapc #'process-class-export (cdr option))))
                 (:initialize
                  (push (build-method (cons 'initialize-instance (cdr option)))
                        methods))
                 (:print
                  (destructuring-bind ((stream &key type identity) &body body)
                      (cdr option)
                    (push (build-method `(print-object (,stream)
                                                       (print-unreadable-object (self ,stream :type ,type :identity ,identity)
                                                         ,@body)))
                          methods))))))
      (mapc #'process-class-option options)

      (if (or exports methods)
          `(progn
             (cl:defclass ,name ,direct-superclasses
               ,defclass-slots
               ,@(remove-if-not (lambda (class-option)
                                  (member (car class-option) '(:documentation :metaclass :default-initargs)))
                  options))
             ,@(when exports
                 `((export ',exports)))
             ,@methods)
          `(cl:defclass ,name ,direct-superclasses
             ,defclass-slots
             ,@(remove-if-not (lambda (class-option)
                                (member (car class-option) '(:documentation :metaclass :default-initargs)))
                options))))))

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

#+test
(macroexpand-1 '(let* ((x 34) (y 44)) (cons x y)))

#+test
(macroexpand-1 '(let* ((res found-p (gethash :key table)))))

#+test
(macroexpand-1 '(let* (((x y) (list 1 2))) (list x y)))

#+test
(let* (((x y) (list 1 2))) (list x y))

#+test
(let* (((x . y) (cons 1 2))) (list x y))

#+test
(let* ((x y z (values 1 2 3))) (list x y z))

#+test
(let* (((x . _) (cons 1 2)))
  x)
