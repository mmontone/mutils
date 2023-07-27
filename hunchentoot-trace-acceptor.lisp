;;; hunchentoot-trace-acceptor --- A Hunchentoot acceptor for tracing HTTP requests.

;; Copyright (C) 2023 Mariano Montone. All rights reserved.

;; This work is licensed under the terms of the MIT license.
;; For a copy, see <https://opensource.org/licenses/MIT>.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Version: 0.1
;; Requires: hunchentoot
;; Keywords: debugging, web, hunchentoot
;;
;;; Commentary:
;;
;; A Hunchentoot acceptor for tracing HTTP requests.
;;
;; Example usage:
;;
;;     (defclass my-acceptor (hunchentoot:easy-acceptor hunchentoot-trace:trace-acceptor)
;;       ())
;;
;; HTTP requests information is printed to `*standard-output*`.
;;
;;; Code:

(require :hunchentoot)

(defpackage :hunchentoot-trace-acceptor
  (:use :cl :hunchentoot)
  (:export
   :trace-acceptor
   :*trace-requests*
   :*trace-session*))

(in-package :hunchentoot-trace-acceptor)

(defvar *trace-requests* t
  "Request tracing is enabled when this is T. Default is T.")
(defvar *trace-session* nil
  "Session tracing is enabled when this is T. Default is NIL.")

(defclass trace-acceptor (acceptor)
  ()
  (:documentation "A Hunchentoot acceptor for tracing requests"))

(defgeneric print-request (request format stream)
  (:documentation "Prints REQUEST to STREAM in FORMAT"))

(defmethod print-request ((request request) format stream)
  (prin1 request stream))

(defgeneric print-session (session format stream)
  (:documentation "Prints SESSION to STREAM in FORMAT"))

(defmethod print-session ((session session) format stream)
  (prin1 session stream))

(defmethod print-session ((session session) (format (eql :text-log)) stream)
  (loop for (key . value) in (hunchentoot::session-data session)
        do (format stream "  ~s: ~s~%" key value)))

(defmethod print-request ((request request) (format (eql :text-log)) stream)
  (format stream "  uri: ~a~%" (request-uri request))
  (format stream "  method: ~a~%" (request-method request))
  (format stream "  headers:~%")
  (loop for (key . value) in (hunchentoot:headers-in request)
        do (format stream "    ~a: ~a~%" key value))
  (when (member (request-method request) '(:patch :post))
    (format stream "  post parameters:~%")
    (loop for (key . value) in (hunchentoot:post-parameters request)
          do (format stream "    ~a: ~a~%" key value))
    (format stream "  request body:~%")
    (write-string (hunchentoot:raw-post-data :request request :force-text t) stream)
    (terpri stream)))

(defmethod acceptor-log-access ((acceptor trace-acceptor) &key return-code)
  (declare (ignore return-code))
  (call-next-method)
  (hunchentoot::with-log-stream (stream (acceptor-access-log-destination acceptor) hunchentoot::*access-log-lock*)
    (when *trace-requests*
      (print-request hunchentoot:*request* :text-log stream))
    (when *trace-session*
      (print-session hunchentoot:*session* :text-log stream))))

(provide :hunchentoot-trace-acceptor)
