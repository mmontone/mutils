;;; plump-xpath --- xpath extension for plump.

;; Copyright (C) 2023 Mariano Montone. All rights reserved.

;; This work is licensed under the terms of the MIT license.  
;; For a copy, see <https://opensource.org/licenses/MIT>.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Version: 0.1
;; Requires: plump, xpath

;;; Commentary:

;; xpath extension for plump.

;;; Code:

(require :plump)
(require :xpath)

(defpackage :plump-xpath
  (:use :cl))

(in-package :plump-xpath)

(defstruct plump-attribute
  name val)

(defun plump-node-attributes (node)
  (loop for name being the hash-keys of (plump-dom:attributes node)
     using (hash-value val)
       collect (make-plump-attribute :name name :val val)))

(defmethod xpath-protocol:node-p-using-navigator ((navi (eql :plump-xpath-navigator)) node)
  (plump-dom:node-p node)
  ;;(plump-dom:element-p node)
  )
(defmethod xpath-protocol:node-equal-using-navigator ((navi (eql :plump-xpath-navigator)) a b)
  nil)

(defmethod xpath-protocol:node-equal-using-navigator ((navi (eql :plump-xpath-navigator)) (a plump-dom:element) (b plump-dom:element))
  (string= (plump-dom:tag-name a)
           (plump-dom:tag-name b)))

(defmethod xpath-protocol:node-equal-using-navigator ((navi (eql :plump-xpath-navigator)) (a plump-dom:text-node) (b plump-dom:text-node))
  (string= (plump-dom:text a)
           (plump-dom:text b)))

(defmethod xpath-protocol:hash-key-using-navigator ((navi (eql :plump-xpath-navigator)) node)
  node)

(defmethod xpath-protocol:parent-node-using-navigator ((navi (eql :plump-xpath-navigator)) node)
  (plump-dom:parent node))

(defmethod xpath-protocol:parent-node-using-navigator ((navi (eql :plump-xpath-navigator)) (node plump:root))
  nil)

(defmethod xpath-protocol:child-pipe-using-navigator ((navi (eql :plump-xpath-navigator)) node)
  (xpath::vector->pipe (plump-dom:children node)))

(defmethod xpath-protocol:child-pipe-using-navigator ((navi (eql :plump-xpath-navigator)) (node plump:doctype))
  nil)

(defmethod xpath-protocol:child-pipe-using-navigator ((navi (eql :plump-xpath-navigator)) (node plump:text-node))
  nil)

(defmethod xpath-protocol:child-pipe-using-navigator ((navi (eql :plump-xpath-navigator)) (node plump:xml-header))
  nil)

(defmethod xpath-protocol:attribute-pipe-using-navigator ((navi (eql :plump-xpath-navigator)) node)
  (xpath::vector->pipe (plump-node-attributes node)))

(defmethod xpath-protocol:node-text-using-navigator ((navi (eql :plump-xpath-navigator)) node)
  (plump-dom:text node))

(defmethod xpath-protocol:node-text-using-navigator ((navi (eql :plump-xpath-navigator)) (node plump-attribute))
  (plump-attribute-val node))

(defmethod xpath-protocol:namespace-uri-using-navigator ((navi (eql :plump-xpath-navigator)) node)
  "")

(defmethod xpath-protocol:local-name-using-navigator ((navi (eql :plump-xpath-navigator)) node)
  (plump-dom:tag-name node))

(defmethod xpath-protocol:local-name-using-navigator ((navi (eql :plump-xpath-navigator)) (node plump-attribute))
  (plump-attribute-name node))

(defmethod xpath-protocol:node-type-p-using-navigator ((navi (eql :plump-xpath-navigator)) node type)
  ;;(format t "~A::~A -> false~%" node type)
  nil)

(defmethod xpath-protocol:node-type-p-using-navigator ((navi (eql :plump-xpath-navigator)) (node plump-dom:element) (type (eql :element)))
  t)

(defmethod xpath-protocol:node-type-p-using-navigator ((navi (eql :plump-xpath-navigator)) (node plump-attribute) (type (eql :attribute)))
  t)

(defmethod xpath-protocol:node-type-p-using-navigator ((navi (eql :plump-xpath-navigator)) (node plump-dom:text-node) (type (eql :text)))
  t)

(defmacro trace-xpath-protocol ()
  (let (code)
    (do-external-symbols (sym :xpath-protocol)
      (when (not (member sym '(xpath-protocol:define-default-method)))
        (push `(trace ,sym) code)))
    `(progn ,@code)))

(provide :plump-xpath)
