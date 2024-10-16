;;; asdf-local --- A module for treating local systems compilation differently from third party systems.

;; Copyright (C) 2023 Mariano Montone. All rights reserved.

;; This work is licensed under the terms of the MIT license.
;; For a copy, see <https://opensource.org/licenses/MIT>.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Version: 0.1
;; Requires: ASDF

;;; Commentary:

;; ASDF-LOCAL signals WARNINGs and STYLE-WARNINGs as errors for ASDF systems considered "local", and compiles third party systems normally.

;; Usage:

;; Set local systems or directories modifying *LOCAL-SYSTEMS*, *LOCAL-DIRECTORIES* and *NON-LOCAL-DIRECTORIES* variables.
;; Then perform an ASDF:OPERATE.
;; Local systems warnings are treated as errors, while non local system warnings are treated as warnings.
;; OPERATE-LOCALLY is a utility function to perform an ASDF operation on an ASDF:SYSTEM, adding the system to the list of local systems in the process.

;; Examples:

;; (operate-locally 'asdf:compile-op :cl-forms :force t)
;; (let ((*fail-on-style-warnings* nil)) (operate-locally 'asdf:load-op :cl-forms :force t))
;; (asdf:operate 'asdf:load-op :ten :force t)
;; (operate-locally 'asdf:load-op :ten :force t)
;; (let ((*local-systems* (list :ten))) (asdf:operate 'asdf:load-op :ten :force t))
;; (let ((*local-directories* (list (asdf:system-source-directory :ten)))) (asdf:operate 'asdf:load-op :ten :force t))

;; To ignore style-warnings, bind *fail-on-style-warnings*:
;; (let ((*fail-on-style-warnings* nil)) (operate-locally 'asdf:load-op :ten :force t))

;;; Code:

(require :asdf)

(defpackage :asdf-local
  (:use :cl)
  (:export
   #:*local-directories*
   #:*non-local-directories*
   #:*asdf-component-being-compiled*
   #:*local-systems*
   #:*local-component-predicates*
   #:*fail-on-style-warnings*
   #:operate-locally))

(in-package :asdf-local)

(defvar *local-directories* ()
  "A list of PATHNAME. List of directories considered local.")
(defvar *non-local-directories* ()
  "A list of PATHNAME. List of directories considered non-local.")
(defvar *asdf-component-being-compiled* nil
  "The current ASDF:COMPONENT being compiled.")
(defvar *local-systems* ()
  "A list of ASDF:SYSTEM considered local.")
(defvar *local-component-predicates* ()
  "A list of FUNCTION-DESIGNATOR that are invoked by COMPONENT-LOCAL-P to determine if a component is local.")
(defvar *fail-on-style-warnings* t
  "When enabled, style-warning errors are signaled as errors for local systems.")

(defun component-local-p (component)
  "Determine if the ASDF COMPONENT should be considered local or not.
A COMPONENT is considered local if its system appears in *LOCAL-SYSTEMS*,
or if its path is subpath or some directory in *LOCAL-DIRECTORIES* and not subpath of some directory in *NON-LOCAL-DIRECTORIES*.
Lastly, function-designators in *LOCAL-COMPONENT-PREDICATES* are invoked."
  (or (member (asdf:component-system component) (mapcar #'asdf:find-system *local-systems*))
      (and
       (some (lambda (local-directory)
               (uiop/pathname:subpathp (asdf:component-pathname component) local-directory))
             *local-directories*)
       (not (some (lambda (non-local-directory)
                    (uiop/pathname:subpathp (asdf:component-pathname component) non-local-directory))
                  *non-local-directories*)))
      (some (lambda (local-component-predicate)
              (funcall local-component-predicate component))
            *local-component-predicates*)))

;; This is we plug our custom behavior
(defmethod asdf/component:around-compile-hook :around ((component asdf:component))
  (lambda (compile-component)
    (let ((*asdf-component-being-compiled* component)
          (asdf::*compile-file-warnings-behaviour* ;; this is the variable used to control how to react to compile-file warnings
            (if (component-local-p component)
                :error
                :warn)))
      (if (and (component-local-p component) (not *fail-on-style-warnings*))
          (uiop/utility:call-with-muffled-conditions
           (lambda () (funcall compile-component)) '(style-warning))
          (funcall compile-component)))))

(defun operate-locally (operation component &rest args)
  "Utility function that includes COMPONENT in the list of *LOCAL-SYSTEMS* before applying the ASDF OPERATION."
  (let ((*local-systems* (cons (asdf:find-system component) *local-systems*)))
    (apply #'asdf:operate operation component args)))

(provide :asdf-local)
