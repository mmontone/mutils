;;; html-actions --- Dynamically register functions as HTTP handlers.

;; Copyright (C) 2023 Mariano Montone. All rights reserved.

;; This work is licensed under the terms of the MIT license.
;; For a copy, see <https://opensource.org/licenses/MIT>.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Version: 0.1
;; Requires: hunchentoot, uuid

;;; Commentary:

;; Dynamically register functions as HTTP handlers.
;;
;; Usage:
;;
;;
;; TODO:
;;  - this code could/should be adapted for Clack.
;;  - change name to http-handler-functions ? http-callbacks ? html-callbacks ?
;;
;;; Code:

(require :hunchentoot)
(require :uuid)

(defpackage html-actions
  (:use :cl)
  (:export #:*register-place*
           #:*use-function-names*
           #:register-handler-function
           #:find-handler-function
           #:find-handler-function-or-error
           #:handler-function-url))

(in-package :html-actions)

;; Valid options are:
;; - session/global - If there's a session started, register in session. Otherwise, then register globally.
;; - session: If there's a session started, register in session. Otherwise, start session and register.
;; - global: Register the handler function globally.

(deftype register-place ()
  '(member :session :global :session/global))

(defvar *register-place* :session/global)
(defvar *use-function-names* t
  "When possible, use function names as ID.")
(defvar *handler-functions*
  (make-hash-table :test 'equalp)
  "Global register of handler functions")

(defun gen-id ()
  (base64:usb8-array-to-base64-string
   (uuid:uuid-to-byte-array
    (uuid:make-v4-uuid))
   :uri t))

(defun function-designator-id (function-designator)
  ;; TODO: use SXHASH to generate unique ids for unique FUNCTION objects ?
  (if (and (symbolp function-designator) *use-function-names*)
      (string-downcase (symbol-name function-designator))
      (gen-id)))

(defun register-handler-function (function-designator &key (place *register-place*) id)
  "FUNCTION-DESIGNATOR can be either a FUNCTION object or a SYMBOL.
Returns the id of the registered FUNCTION-DESIGNATOR."
  (check-type function-designator (or symbol function))
  (check-type place register-place)
  (let ((fid (or id (function-designator-id function-designator))))
    (ecase place
      (:session/global
       (if (hunchentoot:session hunchentoot:*request*)
           (register-handler-function function-designator :place :session :id id)
           (register-handler-function function-designator :place :global :id id)))
      (:session
       (hunchentoot:start-session)
       (when (not (hunchentoot:session-value 'handler-functions))
         (setf (hunchentoot:session-value 'handler-functions)
               (make-hash-table :test 'equalp)))
       (setf (gethash fid (hunchentoot:session-value 'handler-functions))
             function-designator))
      (:global
       (setf (gethash fid *handler-functions*)
             function-designator)))
    fid))

(defun find-handler-function (id &optional (place *register-place*))
  (ecase place
    (:session/global
     (if (hunchentoot:session hunchentoot:*request*)
         (or (find-handler-function id :session)
             (find-handler-function id :global))))
    (:session (gethash id (hunchentoot:session-value 'handler-functions)))
    (:global (gethash id *handler-functions*))))

(defun find-handler-function-or-error (id &optional (place *register-place*))
  (or (find-handler-function id place)
      (progn
        (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
        (hunchentoot:abort-request-handler
         "Handler not found"))))

(defun handler-function-url (function-designator &key (place *register-place*) id)
  (let ((fid (register-handler-function function-designator
                                        :place place
                                        :id id)))
    (format nil "/actions?id=~a" fid)))

(hunchentoot:define-easy-handler (actions-handler :uri "/actions")
    (id)
  (let ((function-designator (find-handler-function-or-error id)))
    (let ((result (funcall function-designator)))
      (if (stringp result)
          result
          "ok"))))

(provide :html-actions)
