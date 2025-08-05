(require :hunchentoot)
(require :html-actions)
(require :spinneret)

(defpackage :htmlx-demo-1
  (:use :cl))

(in-package :htmlx-demo-1)

(defun callback (func)
  (html-actions:handler-function-url func))

(hunchentoot:define-easy-handler (demo :uri "/")
    ()
  (let ((counter 0))
    (spinneret:with-html-string
      (:html
       (:head
        (:title "HTMLX Counter"))
       (:body
        (labels ((counter ()
                   (:div :class "counter" :id "counter"
                         (:h1 (princ-to-string counter))
                         (:button :hx-post (callback
                                            (lambda ()
                                              (incf counter)
                                              (spinneret:with-html-string
                                                (counter))))
                                  :hx-swap "outerHTML"
                                  :hx-target "#counter"
                                  "Increment")
                         (:button :hx-post (callback
                                            (lambda ()
                                              (decf counter)
                                              (spinneret:with-html-string
                                                (counter))))
                                  :hx-swap "outerHTML"
                                  :hx-target "#counter"
                                  "Decrement"))))
          (counter))
        (:script :src "https://cdn.jsdelivr.net/npm/htmx.org@2.0.6/dist/htmx.min.js"))))))

(defun start ()
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 0)))

;; (start)
