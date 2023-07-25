;;; who-templates --- Templating system with CL-WHO. Supports inheritance.

;; Copyright (C) 2023 Mariano Montone. All rights reserved.

;; This work is licensed under the terms of the MIT license.  
;; For a copy, see <https://opensource.org/licenses/MIT>.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Version: 0.1
;; Requires: cl-who

;;; Commentary:

;; Templating system with CL-WHO. Supports inheritance.
;;
;;
;; Base template example:
;;
;;    (deftemplate base-1 ()
;;      (&args title)
;;      (:html
;;       (:head
;;        (:title (who:str (or title "WHO TEMPLATES")))
;;        (block styles
;;          (:link :rel "stylesheet" :href "/bootstrap.css")))
;;       (:body
;;        (block body)
;;        (block scripts))))
;;
;; Render:
;;
;;     (render-template-to-string 'base-1)
;;     (render-template-to-string 'base-1 :title "lala")
;;
;; Inheritance/block overwrite. Calls to parent:
;;
;;     (deftemplate foo (:parent base-1)
;;       (block body
;;         (:h1 (who:str "Foo"))))
;;
;; Render:
;;
;;     (render-template-to-string 'foo)
;;
;; Another example:
;;
;;    (deftemplate bar (:parent base-1)
;;      (block body
;;        (:h1 (who:str "Bar")))
;;      (block styles
;;        (parent)
;;        (:link :rel "stylesheet" :href "/bar.css")))
;;
;; Render:
;;
;;    (render-template-to-string 'bar)
;;
;; Another example:
;;
;;    (deftemplate baz (:parent bar)
;;      (block scripts
;;        (parent)
;;        (:script :type "text/javacript"
;;              (who:str "...javascript code..."))))
;;
;; Render:
;;
;;   (render-template-to-string 'baz)
;;
;; Example with arguments:
;;
;;    (deftemplate hello (:parent base-1)
;;      (block body
;;        (:h1 (who:str (targ :hello)))))
;;
;; Render:
;;
;;    (render-template-to-string 'hello :hello "Hello!!")
;;
;; Another example with arguments:
;;
;;     (deftemplate hello-2 (:parent base-1)
;;       (block body
;;          (&args hello)
;;         (:h1 (who:str hello))
;;         (:h2 (who:str hello))))
;;
;; Render:
;; 
;;     (render-template-to-string 'hello-2 :hello "Hi!!")
;;
;; Include:
;;
;;     (deftemplate snippet ()
;;        (:p (who:str "This stuff has been included")))
;;
;;     (deftemplate include (:parent base-1)
;;        (block body
;;         (include 'snippet)))
;;
;; Render:
;;
;;     (render-template-to-string 'include)
;;
;;; Code:

(require :cl-who)

(defpackage :who-templates
  (:nicknames :whot)
  (:use :cl)
  (:export
   #:deftemplate
   #:block
   #:include
   #:parent
   #:render-template
   #:render-template-to-string
   #:targ
   #:with-targs)
  (:documentation "Templating system with CL-WHO. Supports inheritance."))

(in-package :whot)

(defvar *templates* (make-hash-table))
(defvar *template*)
(defvar *block*)
(defvar *template-args*)
(defvar *out*)

(defclass who-template ()
  ((name :initarg :name
         :accessor template-name
         :type symbol
         :initform (error "Provide the template name"))
   (parent :initarg :parent
           :accessor template-parent
           :type (or null symbol)
           :initform nil)
   (renderer :initarg :renderer
             :accessor template-renderer
             :type (or null function)
             :initform nil)
   (blocks :initarg :blocks
           :accessor template-blocks
           :initform nil)))

(defmethod print-object ((template who-template) stream)
  (print-unreadable-object (template stream :type t :identity t)
    (format stream "~A ~[parent:~A~]"
            (template-name template)
            (template-parent template))))

(defun register-template (template)
  (setf (gethash (template-name template) *templates*) template))

(defun find-template (name)
  (or (gethash name *templates*)
      (error "Template not defined: ~A" name)))

(defmethod initialize-instance :after ((template who-template) &rest initargs)
  (declare (ignore initargs))
  (when (and (template-renderer template)
             (template-parent template))
    (error "Template ~A: Cannot have body and parent at the same time"
           (template-name template)))
  ;; Register the tamplate
  (register-template template))

(defun targ (symbol)
  (getf *template-args* symbol))

(defun args-list-p (form)
  (and (listp form)
       (string= (string-downcase (string (first form))) "&args")))

(defun expand-body (body)
  (if (args-list-p (first body))
      (let ((args (cdr (first body))))
        (list `(let ,(loop for arg in args
                           collect `(,arg (targ ,(intern (string arg) :keyword))))
                 (who:htm ,@(rest body)))))
      body))

(defmacro with-targs (args &body body)
  `(let ,(loop for arg in args
               collect `(,arg (targ ,(intern (string arg) :keyword))))
     (who:htm ,@body)))

(defun collect-replace-blocks (form)
  (let ((blocks '()))
    (let ((new-form (%collect-replace-blocks
                     form
                     (lambda (block) (push block blocks)))))
      (values new-form blocks))))

(defun %collect-replace-blocks (form collect-block)
  (cond
    ((atom form)
     form)
    ((eql (first form) 'block)
     (funcall collect-block (cdr form))
     `(render-block ',(second form)))
    (t
     (loop for part in form
           collect
           (%collect-replace-blocks part collect-block)))))

(defun find-block (name template)
  (let ((block (cdr (assoc name (template-blocks template)))))
    (if block
        (values block template)
        (and (template-parent template)
             (find-block name (find-template (template-parent template)))))))

(defun render-block (block-name)
  (let ((block (find-block block-name *template*)))
    (when block
      (funcall block))))

(defun parent (&optional (block *block*) (template (template-parent *template*)))
  "Render the parent block"
  (let ((parent-template (find-template template)))
    (multiple-value-bind (parent-block *template*)
        (find-block block parent-template)
      (when parent-block
        (funcall parent-block)))))

(defun include (template-name)
  (funcall (template-renderer (find-template template-name))))

(defun find-renderer (template)
  (if (template-parent template)
      (find-renderer (find-template (template-parent template)))
      (template-renderer template)))

(defun parse-template (body)
  (collect-replace-blocks body))

(defmacro deftemplate (name args &body body)
  (multiple-value-bind (new-body blocks)
      (parse-template body)
    `(make-instance 'who-template
                    :name ',name
                    :parent ',(getf args :parent)
                    :renderer ,(when (not (getf args :parent))
                                 `(lambda ()
                                    (who:with-html-output (html *out*)
                                      ,@(expand-body new-body))))
                    :blocks (list ,@(loop for block in blocks
                                          collect `(cons ',(car block)
                                                         (lambda ()
                                                           (let ((*block* ',(car block))
                                                                 (*template* (find-template ',name)))
                                                             (who:with-html-output (html *out*)
                                                               ,@(expand-body (cdr block)))))))))))

(defun render-template-to-string (name &rest args)
  (with-output-to-string (*out*)
    (apply #'render-template name *out* args)))

(defun render-template (name stream &rest args)
  (let ((*template* (find-template name))
        (*template-args* args)
        (*out* stream))
    (let ((renderer (find-renderer *template*)))
      (unless renderer
        (error "Don't know how to render template ~A" name))
      (funcall renderer))))

(provide :who-templates)
