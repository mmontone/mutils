(require :alexandria)
(require :access)

(defpackage clos-star
  (:use :cl)
  (:export #:defclass*
           #:define-condition*
           #:defgeneric*
           #:self))

(in-package :clos-star)

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

(defun concat-symbols (sym1 sym2)
  (intern (format nil "~a~a" sym1 sym2)))

;;(concat-symbols 'foo '-bar)

(defmacro defclass* (name direct-superclasses direct-slots &rest options)
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
                          methods)))
                 (:generate
                  (labels ((process-generate (spec)
                             (if (keywordp spec)
                                 (process-generate (list spec))
                                 (ecase (car spec)
                                   (:initforms
                                    (setf defclass-slots
                                          (mapcar (lambda (slot)
                                                    (if (not (member :initform slot))
                                                        (append slot '(:initform nil))
                                                        slot))
                                                  defclass-slots)))
                                   (:initargs
                                    (setf defclass-slots
                                          (mapcar (lambda (slot)
                                                    (if (not (member :initarg slot))
                                                        (append slot (list :initarg (intern (symbol-name (car slot)) :keyword)))
                                                        slot))
                                                  defclass-slots)))
                                   (:accessors
                                    (destructuring-bind (&key prefix suffix) (rest spec)
                                      (setf defclass-slots
                                            (mapcar (lambda (slot)
                                                      (if (not (member :accessor slot))
                                                          (let ((accessor-name (car slot)))
                                                            (when prefix
                                                              (setf accessor-name (concat-symbols prefix accessor-name)))
                                                            (when suffix
                                                              (setf accessor-name (concat-symbols accessor-name suffix)))
                                                            (append slot (list :accessor accessor-name)))
                                                          slot))
                                                    defclass-slots))))))))
                    (mapc #'process-generate (cdr option)))))))
      (mapc #'process-class-option options)

      (if (or exports methods)
          `(progn
             (defclass ,name ,direct-superclasses
               ,defclass-slots
               ,@(remove-if-not (lambda (class-option)
                                  (member (car class-option) '(:documentation :metaclass :default-initargs)))
                  options))
             ,@(when exports
                 `((export ',exports)))
             ,@methods)
          `(defclass ,name ,direct-superclasses
             ,defclass-slots
             ,@(remove-if-not (lambda (class-option)
                                (member (car class-option) '(:documentation :metaclass :default-initargs)))
                options))))))

(defmacro defgeneric* (name args &rest options))

(defmacro define-condition* (name direct-superclasses direct-slots &rest options))

#+test
(defclass* my-class ()
  ((name :initarg :name
         :accessor my-class-name
         :export (:accessor :slot)
         :required t)
   (lastname :required "Enter the last name")
   (lalal))

  (:documentation "This is a great class.")

  ;; determines exports unless :export is explicitly specified in the slots
  (:export :all :class-name :accessors :slots)

  (:dot-syntax t)

  (:generate :initforms (:accessors :prefix my-class-)
             :initargs)

  (:method print-object (stream)
    (print-unreadable-object (self stream :type t :identity t)
      (write-string self.name stream)))

  (:print (stream :type t :identity t)
          (write-string self.my-class-name stream))

  (:initialize :after (&rest initargs)
               (call-next-method)))

(provide :clos-star)
