(require :alexandria)
(require :access)

(defpackage clos-star
  (:use :cl)
  (:export #:defclass*
           #:define-condition*
           #:defgeneric*))

(in-package :clos-star)

(defun map-plist (func plist)
  (let ((new-plist ()))
    (alexandria:doplist (key val plist)
      (let ((res (multiple-value-list (funcall func key val))))
        (when (keywordp (first res))
          (push (first res) new-plist)
          (push (second res) new-plist))))
    (nreverse new-plist)))

(map-plist (lambda (key val)
             (when (not (eq key :foo))
               (values key val)))
           '(:bar 22 :foo 33 :baz 22))

(defun maybe-progn (&rest forms)
  (let ((progn-forms (remove nil forms)))
    (cond
      ((> (length progn-forms) 1)
       `(progn ,@progn-forms))
      ((= 1 (length progn-forms))
       (car progn-forms)))))

(maybe-progn 'foo 'bar)
(maybe-progn 'foo nil)


(defparameter *star-class-options* '(:required :export :dot-syntax :method :print :initialize))

(defmacro defclass* (name direct-superclasses direct-slots &rest options)
  (let ((methods (list))
        (exports (list))
        defclass-slots)
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

    (flet ((process-class-option (option)
             (case (car option)
               (:method (destructuring-bind (name args &body body)
                            (cdr option)
                          (push `(defmethod ,name ,args ,@body) methods)))
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
                  (mapc #'process-class-export (cdr option)))))))
      (mapc #'process-class-option options)
      
      (maybe-progn
       `(defclass ,name ,direct-superclasses
          ,defclass-slots
          ,@(remove-if-not (lambda (class-option)
                             (member (car class-option) '(:documentation :metaclass)))
             options))
       (when exports
         `(export ',exports))
       methods)      
      )))

(defmacro defgeneric* (name args &rest options))

(defmacro define-condition* (name direct-superclasses direct-slots &rest options))

(defclass* my-class ()
  ((name :initarg :name
         :accessor my-class-name
         :export (:accessor :slot)
         :required t)
   (lastname :required "Enter the last name"))

  (:documentation "This is a great class.")

  (:export :all :class-name :accessors :slots) ;; exports unless :export nil is explicitly specified

  (:dot-syntax t)

  (:method print-object (stream)
    (print-unreadable-object (self stream :type t :identity t)
      (write-string self.name stream)))

  (:print (stream :type t :identity t)
          (write-string self.my-class-name stream))

  (:initialize :after (&rest initargs)
               (call-next-method)))
