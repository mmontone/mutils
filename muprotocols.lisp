;;; muprotocols --- An implementation of protocols for Common Lisp.

;; Copyright (C) 2023 Mariano Montone. All rights reserved.

;; This work is licensed under the terms of the MIT license.  
;; For a copy, see <https://opensource.org/licenses/MIT>.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Version: 0.1

;;; Commentary:

;; An implementation of protocols for Common Lisp.

;;; Code:

(defpackage :muprotocols
  (:use :cl)
  (:export
   #:defprotocol
   #:implement-protocol
   #:check-implements
   #:implements))

(in-package :muprotocols)



(provide :muprotocols)
