;;; html2who --- Parse HTML and create cl-who source code.

;; Copyright (C) 2023 Mariano Montone. All rights reserved.

;; This work is licensed under the terms of the MIT license.  
;; For a copy, see <https://opensource.org/licenses/MIT>.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Version: 0.1
;; Requires: cl-who, cl-html5-parser

;;; Commentary:

;; Parse HTML and create cl-who source code.

;; Usage:

;;(html5-parser:parse-html5 #p"/vagrant/admin/index.html" :dom :who :strictp nil)
;;(html5-parser:parse-html5-fragment #p"/vagrant/admin/index.html" :dom :who :strictp nil)
;;(html5-parser:parse-html5 #p"/vagrant/admin/index.html" :dom :xmls :strictp nil)
;;; Code:

(require :cl-who)
(require :cl-html5-parser)

(defpackage html2who
  (:use :cl))

(in-package :html2who)

(defun empty-string-p (string)
  (let ((empty-chars (list #\space #\newline #\return #\tab)))
    (every (lambda (char)
             (member char empty-chars))
           string)))

(defmethod html5-parser:transform-html5-dom
    ((to-type (eql :who)) node
     &key namespace comments)
  "Convert a node into a cl-who tree"
  (labels ((node-to-who (node)
             (ecase (html5-parser:node-type node)
               (:document
                (let (root)
                  (html5-parser:element-map-children (lambda (n)
                                                       (when (string= (html5-parser:node-name n) "html")
                                                         (setf root n)))
                                                     node)
                  (assert root)
                  (node-to-who root)))
               (:document-fragment
                (let (who-nodes)
                  (html5-parser:element-map-children (lambda (node)
                                                       (push (node-to-who node)
                                                             who-nodes))
                                                     node)
                  (nreverse who-nodes)))
               (:element
                (let (attrs children)
                  (html5-parser:element-map-attributes (lambda (name namespace value)
                                                         (declare (ignore namespace))
                                                         (push (list (intern (string-upcase name) :keyword) value) attrs))
                                                       node)
                  (html5-parser:element-map-children (lambda (c)
                                                       (push c children))
                                                     node)
                  `(
                    ,(intern (string-upcase (html5-parser:node-name node)) :keyword)
                    ,@(alexandria:flatten attrs)
                    ,@(mapcar (lambda (c)
                                   (node-to-who c))
                                 (nreverse children)))))
               (:text
                (if (empty-string-p (html5-parser:node-value node))
                    nil
                    `(who:str ,(html5-parser:node-value node))))
               (:comment
                (when comments
                  (list :comment nil (html5-parser:node-value node)))))))
    (node-to-who node)))

(defun remove-blanks (tree)
  (if (atom tree)
      tree
      (mapcar #'remove-blanks
              (remove nil tree))))

(defun html-generate-who (pathname &key (remove-blanks t))
  (let ((who
          (html5-parser:parse-html5 pathname :dom :who :strictp nil)))
    (if remove-blanks
        (remove-blanks who)
        who)))

(defmethod html5-parser:transform-html5-dom
    ((to-type (eql :text)) node
     &key namespace comments)
  "Convert a node into text"
  (labels ((node-to-text (node)
             (ecase (html5-parser:node-type node)
               (:document
                (let (root)
                  (html5-parser:element-map-children
                   (lambda (n)
                     (when (string= (html5-parser:node-name n) "html")
                       (setf root n)))
                   node)
                  (assert root)
                  (node-to-text root)))
               (:document-fragment
                (with-output-to-string (html)
                  (html5-parser:element-map-children
                   (lambda (node)
                     (write-string (node-to-text node) html))
                   node)))
               (:element
                (with-output-to-string (html)
                  (format html "<~A" (html5-parser:node-name node))
                  (html5-parser:element-map-attributes
                   (lambda (name namespace value)
                     (declare (ignore namespace))
                     (format html " ~A=\"~A\"" name value))
                   node)
                  (write-string ">" html)
                  (html5-parser:element-map-children
                   (lambda (c)
                     (write-string (node-to-text c) html))
                   node)
                  (format html "</~A>" (html5-parser:node-name node))))
               (:text
                (or (html5-parser:node-value node) ""))
               (:comment
                (or (and comments
                         (format nil "<!--~A-->" (html5-parser:node-value node)))
                    "")))))
    (node-to-text node)))

(provide :html2who)
