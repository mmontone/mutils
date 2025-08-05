(require :hunchentoot)
(require :html-actions)
(require :spinneret)

(defpackage :htmlx-demo-2
  (:use :cl))

(in-package :htmlx-demo-2)

(defun callback (func)
  (html-actions:handler-function-url func))

(defmacro html-region (&body body)
  (let ((region-name (string-downcase (princ-to-string (gensym)))))
    `(flet ((render-region ()
              (spinneret:with-html
                (:div :id ,region-name
                      ,@body))))
       (render-region)
       (cons ,region-name
             #'render-region))))

(defun update-html-region (html-region)
  (spinneret:with-html-string
    (funcall (cdr html-region))))

(defun hx-target (html-region)
  (format nil "#~a" (car html-region)))

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
                         (let ((display (html-region
                                          (:h1 (princ-to-string counter)))))
                           (:button :hx-post (callback
                                              (lambda ()
                                                (incf counter)
                                                (update-html-region display)))
                                    :hx-swap "outerHTML"
                                    :hx-target (hx-target display)
                                    "Increment")
                           (:button :hx-post (callback
                                              (lambda ()
                                                (decf counter)
                                                (update-html-region display)))
                                    :hx-swap "outerHTML"
                                    :hx-target (hx-target display)
                                    "Decrement")))))
          (counter))
        (:script :src "https://cdn.jsdelivr.net/npm/htmx.org@2.0.6/dist/htmx.min.js"))))))

(defun start ()
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 0)))

;; (start)
