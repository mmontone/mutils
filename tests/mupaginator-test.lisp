(require :hunchentoot)
(require :easy-routes)
(require :mutils)
(require :mupaginator)
(require :find-port)
(require :cl-who)

(defpackage :mupaginator-test
  (:use :cl :easy-routes)
  (:export #:start))

(in-package :mupaginator-test)

(defvar *page-size* 10)
(defvar *stylesheets* '())

(defmacro with-html (&body body)
  `(who:with-html-output-to-string (html)
     (:html
      :lang "en"
      (:head
       (:title (who:str "Paginator demo"))
       (dolist (stylesheet *stylesheets*)
         (who:htm (:link :rel "stylesheet" :href stylesheet)))
       (:body
        ,@body)))))

(defroute examples "/"
    ()
  (with-html
    (:ul
     (:li (:a :href (easy-routes:genurl 'vanilla-pagination-test)
              (who:str "Vanilla")))
     (:li (:a :href (easy-routes:genurl 'bootstrap-pagination-test)
              (who:str "Bootstrap")))
     (:li (:a :href (easy-routes:genurl 'w3css-pagination-test)
              (who:str "W3CSS")))
     )))

(defroute vanilla-pagination-test "/vanilla"
    ((page :parameter-type 'integer :init-form 1))
  (let ((pagination (mupaginator:make-pagination :current page :source (list-all-packages))))
    (with-html
      (:ul
       (dolist (item (mupaginator:pagination-current-items pagination))
         (who:htm (:li (who:str (package-name item))))))
      (mupaginator:print-pagination-bootstrap
       pagination
       :href (lambda (page-nr)
               (easy-routes:genurl 'vanilla-pagination-test :page page-nr))
       :stream html))))

(defroute bootstrap-pagination-test "/bootstrap"
    ((page :parameter-type 'integer :init-form 1))
  (let* ((pagination (mupaginator:make-pagination :current page :source (list-all-packages))))
    (let ((*stylesheets* '("https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css")))
      (with-html
        (:ul :class "list-group"
             (dolist (item (mupaginator:pagination-current-items pagination))
               (who:htm (:li :class "list-group-item" (who:str (package-name item))))))
        (mupaginator:print-pagination-bootstrap
         pagination
         :href (lambda (page-nr)
                 (easy-routes:genurl 'bootstrap-pagination-test :page page-nr))
         :stream html)))))

(defroute w3css-pagination-test "/w3css"
    ((page :parameter-type 'integer :init-form 1))
  (let* ((pagination (mupaginator:make-pagination :current page :source (list-all-packages))))
    (let ((*stylesheets* '("https://www.w3schools.com/w3css/4/w3.css")))
      (with-html
        (:ul :class "w3-ul"
             (dolist (item (mupaginator:pagination-current-items pagination))
               (who:htm (:li (who:str (package-name item))))))
        (mupaginator:print-pagination-w3css
         pagination
         :href (lambda (page-nr)
                 (easy-routes:genurl 'w3css-pagination-test :page page-nr))
         :stream html)))))

(defun start ()
  (hunchentoot:start (make-instance 'easy-routes-acceptor :port (find-port:find-port))))
