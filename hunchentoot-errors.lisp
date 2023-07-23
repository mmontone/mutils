;;; hunchentoot-errors --- Augments Hunchentoot error pages and logs with request and session information.

;; Copyright (C) 2023 Mariano Montone. All rights reserved.

;; This work is licensed under the terms of the MIT license.  
;; For a copy, see <https://opensource.org/licenses/MIT>.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Version: 0.1
;; Requires: hunchentoot

;;; Commentary:

;; Augments Hunchentoot error pages and logs with request and session information.
;; ### Usage

;; Subclass your acceptor from `HUNCHENTOOT-ERRORS:ERRORS-ACCEPTOR`.

;; When `hunchentoot:*show-lisp-errors-p*` is on, you get HTTP request and session information printed in errors pages and logs, like:

;; ```
;; Backtrace for: #<SB-THREAD:THREAD "hunchentoot-worker-127.0.0.1:46428" RUNNING {1002007DE3}>
;; 0: (TRIVIAL-BACKTRACE:PRINT-BACKTRACE-TO-STREAM #<SB-IMPL::CHARACTER-STRING-OSTREAM {1003C82953}>)
;; 1: ((FLET "FORM-FUN-4" :IN HUNCHENTOOT::GET-BACKTRACE))
;; 2: (HUNCHENTOOT::GET-BACKTRACE)
;; 3: ((FLET "H0" :IN HUNCHENTOOT:HANDLE-REQUEST) #<SIMPLE-ERROR "sdf" {1003C827F3}>)
;; 4: (SB-KERNEL::%SIGNAL #<SIMPLE-ERROR "sdf" {1003C827F3}>)
;; 5: (ERROR "sdf")
;; 6: (INVOICE-ENGINE::ADMIN/USERS/CREATE)
;; 7: ((LAMBDA NIL :IN EASY-ROUTES::PROCESS-ROUTE))
;; 8: (EASY-ROUTES::CALL-WITH-DECORATORS NIL #<CLOSURE (LAMBDA NIL :IN EASY-ROUTES::PROCESS-ROUTE) {1003C7C57B}>)
;; 9: ((LAMBDA NIL :IN EASY-ROUTES::CALL-WITH-DECORATORS))
;; ...
;; 31: (SB-THREAD::CALL-WITH-MUTEX #<CLOSURE (FLET SB-THREAD::WITH-MUTEX-THUNK :IN SB-THREAD::NEW-LISP-THREAD-TRAMPOLINE) {7FD95C27ED9B}> #<SB-THREAD:MUTEX "thread result lock" owner: #<SB-THREAD:THREAD "hunchentoot-worker-127.0.0.1:46428" RUNNING {1002007DE3}>> NIL T NIL)
;; 32: (SB-THREAD::NEW-LISP-THREAD-TRAMPOLINE #<SB-THREAD:THREAD "hunchentoot-worker-127.0.0.1:46428" RUNNING {1002007DE3}> NIL #<CLOSURE (LAMBDA NIL :IN BORDEAUX-THREADS::BINDING-DEFAULT-SPECIALS) {1001FF7FBB}> NIL)
;; 33: ("foreign function: call_into_lisp")
;; 34: ("foreign function: new_thread_trampoline")

;; HTTP REQUEST:
;;   uri: /admin/users/new
;;   method: POST
;;   post parameters:
;;     name: asdf
;;     username: asdf
;;     email: sdf@asdfasdf.com
;;     password: asdfasdf

;; SESSION:
;;   FLASH-MESSAGES: NIL
;;   ROLE: "superadmin"
;;   USER: 3
;;   FORWARD-URL: "/"
;; ```

;;; Code:

(require :hunchentoot)

(defpackage #:hunchentoot-errors
  (:use #:cl #:hunchentoot)
  (:export #:errors-acceptor))

(in-package #:hunchentoot-errors)

(defclass errors-acceptor (acceptor)
  ((log-request :initarg :log-request
                :accessor log-requestp
                :initform t
                :documentation "When enabled, request information is written to the log.")
   (debug-request :initarg :debug-request
                  :accessor debug-requestp
                  :initform t
                  :documentation "When enabled, request information is printed in Hunchentoot status error pages.")
   (log-session :initarg :log-session
                :accessor log-sessionp
                :initform t
                :documentation "When enabled, session information is written to the log.")
   (debug-session :initarg :debug-session
                  :accessor debug-sessionp
                  :initform t
                  :documentation "When enabled, session information is printed in Hunchentoot status error pages."))
  (:documentation "Subclasses of this acceptor augment Hunchentoot error pages and logs with request and session information."))

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
          do (format stream "    ~a: ~a~%" key value))))

(defmethod print-request ((request request) (format (eql :html)) stream)
  (format stream "<div class=\"request\">")
  (format stream "<h1>Request</h1>~%")
  (format stream "<p><b>Uri:</b> ~a</p>~%" (request-uri request))
  (format stream "<p><b>Method:</b> ~a</p>~%" (request-method request))
  (format stream "<p><b>Headers:</b>~%<ul>")
  (loop for (key . value) in (hunchentoot:headers-in request)
        do (format stream "<li><i>~a:</i> ~a</li>~%" key value))
  (format stream "</ul></p>")
  (when (member (request-method request) '(:patch :post))
    (format stream "<p><b>Post parameters:</b>~%<ul>")
    (loop for (key . value) in (hunchentoot:post-parameters request)
          do (format stream "<li><i>~a:</i> ~a</li>~%" key value))
    (format stream "</ul></p>"))
  (format stream "</div>"))

(defmethod print-session ((session session) (format (eql :html)) stream)
  (format stream "<div class=\"session\">")
  (format stream "<h1>Session</h1>~%<ul>")
  (loop for (key . value) in (hunchentoot::session-data session)
        do (format stream "<li>~s: ~s~%</li>" key value))
  (format stream "</ul></div>"))

(defun accept-format (&optional (content-type (hunchentoot:header-in "accept" *request*)))
  (or (and content-type
           (let ((accepts (mimeparse:best-match
                           (list "text/lisp"
                                 "application/lisp"
                                 "text/xml"
                                 "application/xml"
                                 "text/html"
                                 "application/json")
                           content-type)))
             (string-case:string-case (accepts :default :text)
               ("text/xml" :xml)
               ("application/xml" :xml)
               ("text/html" :html)
               ("application/json" :json)
               ("text/lisp" :sexp)
               ("application/lisp" :sexp))))
      :text))

(defgeneric acceptor-log-error (stream acceptor log-level format-string &rest format-arguments))

(defmethod acceptor-log-error (stream (acceptor errors-acceptor) log-level format-string &rest format-arguments)
  ;; This snippet is from original Hunchentoot:
  (format stream "[~A~@[ [~A]~]] ~?~%"
          (hunchentoot::iso-time) log-level
          format-string format-arguments)

  ;; This part is hunchentoot-errors specific:
  (when (and (log-requestp acceptor)
             (boundp '*request*))
    (format stream "HTTP REQUEST:~%")
    (print-request *request* :text-log stream))
  (when (and (log-sessionp acceptor)
             (boundp '*session*)
             (not (null *session*)))
    (format stream "~%SESSION:~%")
    (print-session *session* :text-log stream))
  (terpri stream))

(defmethod acceptor-log-message ((acceptor errors-acceptor) log-level format-string &rest format-arguments)
  "Sends a formatted message to the destination denoted by (ACCEPTOR-MESSAGE-LOG-DESTINATION ACCEPTOR).
FORMAT and ARGS are as in FORMAT.
LOG-LEVEL is a keyword denoting the log level or NIL in which case it is ignored."
  (if (not (eq log-level :error))
      (call-next-method)
      ;; else
      (hunchentoot::with-log-stream (stream (acceptor-message-log-destination acceptor) hunchentoot::*message-log-lock*)
	(handler-case
            (apply #'acceptor-log-error stream acceptor log-level format-string format-arguments)
	  (error (e)
            (ignore-errors
             (format *trace-output* "error ~A while writing to error log, error not logged~%" e)))))))

(defmethod acceptor-status-message ((acceptor errors-acceptor) http-status-code &key &allow-other-keys)
  (if (not *show-lisp-errors-p*)
      (call-next-method)
      (concatenate
       'string
       (call-next-method)
       (with-output-to-string (msg)
         (let ((format (accept-format)))
           (when (and (debug-requestp acceptor)
                      (boundp '*request*)
                      (not (null *request*)))
             (print-request *request* format msg))
           (when (and (debug-sessionp acceptor)
                      (boundp '*session*)
                      (not (null *session*)))
             (print-session *session* format msg)))))))

(provide :hunchentoot-errors)
