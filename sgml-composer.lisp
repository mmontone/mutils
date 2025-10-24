(defpackage :sgml-composer
  (:use :cl)
  (:export #:make-element
           #:sgml
           #:with-sgml
           #:element
           #:element-tag
           #:element-attributes
           #:element-children))

(in-package :sgml-composer)

(defclass element ()
  ((tag :initarg :tag
        :accessor element-tag)
   (attributes :initarg :attributes
               :accessor element-attributes)
   (children :initarg :children
             :accessor element-children)))

(defmethod print-object ((element element) stream)
  (print-unreadable-object (element stream :type nil :identity nil)
    (format stream "~a" (element-tag element))))

(defun make-element (tag attributes children)
  (let ((children (if (atom children)
                      (list children)
                      children)))
    (make-instance 'element
                   :tag tag
                   :attributes attributes
                   :children (remove nil children))))

(defun transform-sgml-forms (form)
  "Forms that start with a keyword are transformed to MAKE-ELEMENT."
  (if (atom form)
      form
      (if (keywordp (first form))
          (destructuring-bind (tag args &body children) form
            `(make-element ,tag
                           (list ,@args)
                           ,(cond
                              ((= (length children) 0) "")
                              ((= (length children) 1)
                               (transform-sgml-forms (car children)))
                              ((> (length children) 1)
                               `(list ,@(mapcar #'transform-sgml-forms children))))))
          (mapcar #'transform-sgml-forms form))))

(defmacro sgml (&body body)
  `(progn ,@(transform-sgml-forms body)))

(defun write-sgml (element &optional destination)
  (mutils-utils:with-output-to-destination (out destination)
    (if (typep element 'element)
        (let ((tag-name (string-downcase (symbol-name (element-tag element)))))
          (progn
            (write-string "<" out)
            (write-string tag-name out)
            (when (element-attributes element)
              (let ((key-p t))
                (dolist (attribute (element-attributes element))
                  (if key-p
                      (progn
                        (write-char #\space out)
                        (write-string (string-downcase (symbol-name attribute)) out)
                        (write-string "=\"" out))
                      (progn
                        (princ attribute out)
                        (write-char #\" out)))
                  (setf key-p (not key-p)))))
            (write-string ">" out)
            (dolist (child (element-children element))
              (write-sgml child out))
            (format out "</~a>" tag-name)))
        (princ element out))))

(defun write-sgml-indented (element &optional destination (indent-offset 2))
  (let ((indent-level 0))
    (mutils-utils:with-output-to-destination (out destination)
      (labels ((indent ()
                 (dotimes (x indent-level)
                   (write-char #\space out)))
               (write-sgml-indent (element)
                 (indent)
                 (if (typep element 'element)
                     (let ((tag-name (string-downcase (symbol-name (element-tag element)))))
                       (progn
                         (write-string "<" out)
                         (write-string tag-name out)
                         (when (element-attributes element)
                           (let ((key-p t))
                             (dolist (attribute (element-attributes element))
                               (if key-p
                                   (progn
                                     (write-char #\space out)
                                     (write-string (string-downcase (symbol-name attribute)) out)
                                     (write-string "=\"" out))
                                   (progn
                                     (princ attribute out)
                                     (write-char #\" out)))
                               (setf key-p (not key-p)))))
                         (write-string ">" out)
                         (terpri out)
                         (incf indent-level indent-offset)
                         (dolist (child (element-children element))
                           (write-sgml-indent child))
                         (terpri out)
                         (decf indent-level indent-offset)
                         (indent)
                         (format out "</~a>" tag-name)
                         ))
                     (princ element out))))
        (write-sgml-indent element)))))

(sgml (:div (:class "foo") "hello"))

(write-sgml (sgml (:div (:class "foo") "hello")))

(write-sgml
 (sgml (:div (:class "foo")
             (:p () "bah")
             "hello")))

(write-sgml
 (sgml (:div (:class "foo")
             (loop for message in '("hello" "cruel" "world")
                   collect
                   (:p (:type message) message)))))

(write-sgml
 (sgml (:div ()
             (when t
               (:span () "Yes"))
             (when nil
               (:span () "No")))))

(write-sgml-indented
 (sgml (:div (:class "foo")
             (loop for message in '("hello" "cruel" "world")
                   collect
                   (:p (:type message) message))))
 *standard-output*)
