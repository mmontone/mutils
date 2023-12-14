;;; extended-trace --- A TRACE replacement with some extra report options.

;; Copyright (C) 2023 Mariano Montone. All rights reserved.

;; This work is licensed under the terms of the MIT license.
;; For a copy, see <https://opensource.org/licenses/MIT>.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Version: 0.1
;; Requires: trivial-backtrace, alexandria, bordeaux-threads

;;; Commentary:

;; A TRACE replacement with some extra report options.
;;
;; NOTE: this module is WIP and very lacking.
;;

;;; Code:

(require :trivial-backtrace)
(require :alexandria)
(require :bordeaux-threads)

(defpackage extended-trace
  (:use :cl)
  (:shadow :trace)
  (:export #:trace))

(in-package :extended-trace)

#+sbcl
(defmacro trace (func &rest specs)
  "Extended trace.

Additional :REPORT options:

- :BACKTRACE: Outputs backtrace. SBCL only.
- :THREAD: Outputs current thread. SBCL only."
  
  ;; Update the implementation to support several report options at once, like: :report (:backtrace :thread)
  (let ((report (getf specs :report)))
    (cond
      ;; Support :backtrace report option in SBCL
      ((and (eql report :backtrace)
            (string= (lisp-implementation-type) "SBCL"))
       `(cl:trace ,func :condition
                  (progn (trivial-backtrace:print-backtrace nil) t)
                  ,@(alexandria:remove-from-plist specs :report)))
      ;; Output current thread name when :report :thread
      ((and (eql report :thread)
            (string= (lisp-implementation-type) "SBCL"))
       `(cl:trace ,func :condition
                  (progn
                    (format *trace-output* "in ~a:~%" (bt:thread-name (bt:current-thread)))
                    t)
                  ,@(alexandria:remove-from-plist specs :report)))
      (t `(cl:trace ,func ,@specs)))))

(provide :extended-trace)
