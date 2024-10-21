;;; mupaginator --- A helper package for pagination of collections.
;;
;; https://www.zacfukuda.com/blog/pagination-algorithm
;;
;; Version: 0.1
;; Requires: cl-who, alexandria
;;
;;; Commentary:
;;
;; Helper package for implementing pagination of collections.
;;
;; Usage:
;;
;; Create a PAGINATION object using PAGINATE function, passing the current page and the total number of pages.
;; Then print that object to an HTML stream, using of the PRINT-PAGINATION functions, and passing HREF or ON-CLICK handlers for resolving urls/actions for the page buttons.
;;
;; ```lisp
;; (defroute pagination-test "/"
;;     ((page :parameter-type 'integer :init-form 1))
;;   (let* ((total (floor (/ (length *countries*) *page-size*)))
;;          (pagination (mupaginator:paginate page total))
;;          (items (mapcar #'cadr
;;                         (apply #'subseq *countries* (multiple-value-list (mupaginator:page-start-end page *page-size* (length *countries*)))))))
;;     (with-html
;;       (:ul
;;        (dolist (item items)
;;          (who:htm (:li (who:str item)))))
;;       (mupaginator:print-pagination-bootstrap
;;        pagination
;;        :href (lambda (page-nr)
;;                (easy-routes:genurl 'pagination-test :page page-nr))
;;        :stream html))))
;; ```
;;
;;; Code:

(require :cl-who)
(require :alexandria)

(defpackage :mupaginator
  (:use :cl)
  (:export #:pagination
           #:paginate
           #:print-pagination
           #:print-pagination-html
           #:print-pagination-bootstrap
           #:print-pagination-w3css
           #:page-start-end
           #:pagination-current
           #:pagination-next
           #:pagination-prev
           #:pagination-pages
           #:pagination-total))

(in-package :mupaginator)

(defstruct pagination
  current next prev pages total)

(defun paginate (current total &key (use-ellipsis t) (padding 2)
                                 (include-first-and-last t))
  "Create a PAGINATION structure that can be then printed to HTML.
Args:
- CURRENT is the current page number, between 1 and TOTAL.
- TOTAL is the total number of pages.
- USE-ELLIPSIS: an :ELLIPSIS keyword is included when appropiate when enabled.
- PADDING: the list of buttons has length (PADDING * 2) + 1.
- INCLUDE-FIRST-AND-LAST: buttons for first and last page are included."  
  (let ((prev (if (= current 1) nil (1- current)))
        (next (if (= current total) nil (1+ current)))
        (pages '()))
    (when (= current total 1)
      (return-from paginate
        (make-pagination :current current
                         :next next
                         :prev prev
                         :pages pages
                         :total total)))

    (let* ((range (cond
                    ((< (- current padding) 1)
                     (cons 1 (+ padding padding 1)))
                    ((> (+ current padding) total)
                     (cons (- total padding padding) total))
                    (t
                     (cons (- current padding)
                           (+ current padding)))))
           (r1 (car range))
           (r2 (cdr range)))

      ;; first page
      (when (> r1 1)
        (when use-ellipsis
          (when include-first-and-last
            (alexandria:appendf pages '(1)))
          (alexandria:appendf pages (list :ellipsis))))

      ;; pages slice
      (loop for i from r1 to r2
            do (alexandria:appendf pages (list i)))

      (when (< r2 total)
        (when use-ellipsis
          (alexandria:appendf pages (list :ellipsis))

          ;; last page
          (when include-first-and-last
            (alexandria:appendf pages (list total)))))

      (assert (member current pages))
      (make-pagination :current current
                       :next next
                       :prev prev
                       :pages pages
                       :total total)
      )))

;; (paginate 1 10)
;; (paginate 2 10)
;; (paginate 50 100)
;; (paginate 3 10)
;; (paginate 4 10)
;; (paginate 5 10)
;; (paginate 5 10 :padding 3)
;; (paginate 5 10 :padding 3 :use-ellipsis nil)

(defun print-pagination (pagination &optional (stream *standard-output*))
  "Debug function for printing a text representation of PAGINATION."
  (dolist (button (pagination-pages pagination))
    (if (eq button :ellipsis)
        (write-string "..." stream)
        (if (= button (pagination-current pagination))
            (format stream "[~a]" button)
            (format stream "~a" button)))
    (write-char #\space stream)))

(defun print-pagination-html (pagination
                              &key
                                href
                                on-click
                                (stream *standard-output*)
                                (first-and-last-buttons t)
                                (prev-and-next-buttons t))
  "Print PAGINATION to a vanilla HTML STREAM.
Args:
- HREF: a FUNCTION-DESIGNATOR that is called with a page number argument and should return the URL string for that page for the buttons.
- ON-CLICK: a FUNCTION-DESIGNATOR that is called with a page number argument and should return a Javascript string to use for the onclick event on the buttons.
- STREAM: the stream where to write the HTML to.
- FIRST-AND-LAST-BUTTONS: render first and last page buttons.
- PREV-AND-NEXT-BUTTONS: render previous and next page buttons."
  (who:with-html-output (html stream)
    (:div :class "pagination"
          (when first-and-last-buttons
            (who:htm
             (:a :href (when href (funcall href 1))
                 :class (when (= (pagination-current pagination) 1) "disabled")
                 :onclick (when on-click (funcall on-click 1))
                 :alt "First"
                 (who:str (who:escape-string "<<")))))
          (when prev-and-next-buttons
            (who:htm
             (:a :class (when (not (pagination-prev pagination)) "disabled")
                 :href (when (and (pagination-prev pagination) href) (funcall href (pagination-prev pagination)))
                 :onclick (when (and (pagination-prev pagination) on-click) (funcall on-click (pagination-prev pagination)))
                 :alt "Previous"
                 (who:str (who:escape-string "<")))))
          (dolist (page (pagination-pages pagination))
            (if (eq page :ellipsis)
                (who:htm (:span :class "ellipsis" (who:str "...")))
                (who:htm
                 (:a :class (when (= page (pagination-current pagination)) "active")
                     :href (when href (funcall href page))
                     :onclick (when on-click (funcall on-click page))
                     (who:str page)))))
          (when prev-and-next-buttons
            (who:htm
             (:a :class (when (not (pagination-next pagination)) "disabled")
                 :href (when (and (pagination-next pagination) href)
                         (funcall href (pagination-next pagination)))
                 :onclick (when (and (pagination-next pagination) on-click)
                            (funcall on-click (pagination-next pagination)))
                 :alt "Next"
                 (who:str (who:escape-string ">")))))
          (when first-and-last-buttons
            (who:htm
             (:a :class (when (= (pagination-current pagination)
                                 (pagination-total pagination))
                          "disabled")
                 :href (when href (funcall href (pagination-total pagination)))
                 :onclick (when on-click (funcall on-click (pagination-total pagination)))
                 :alt "Last"
                 (who:str (who:escape-string ">>")))))
          )))

#+example
(with-output-to-string (s)
  (print-pagination-to-html
   (paginate 2 10)
   (lambda (page)
     (format nil "/page/~a" page))
   :stream s))

;; <nav aria-label="Page navigation example">
;;   <ul class="pagination justify-content-center">
;;     <li class="page-item disabled">
;;       <a class="page-link" href="#" tabindex="-1">Previous</a>
;;     </li>
;;     <li class="page-item"><a class="page-link" href="#">1</a></li>
;;     <li class="page-item"><a class="page-link" href="#">2</a></li>
;;     <li class="page-item"><a class="page-link" href="#">3</a></li>
;;     <li class="page-item">
;;       <a class="page-link" href="#">Next</a>
;;     </li>
;;   </ul>
;; </nav>

;; https://getbootstrap.com/docs/4.1/components/pagination/?

(defun print-pagination-bootstrap (pagination
                                   &key
                                     href on-click
                                     (stream *standard-output*)
                                     (first-and-last-buttons t)
                                     (prev-and-next-buttons t))
  "Like PRINT-PAGINATION-HTML, but for Bootstrap framework."
  (who:with-html-output (html stream)
    (:ul :class "pagination"
         (when first-and-last-buttons
           (who:htm
            (:li :class "page-item"
                 (:a :class "page-link"
                     :href (when href (funcall href 1))
                     :onclick (when on-click (funcall on-click 1))
                     (who:str (who:escape-string "<<"))))))
         (when (and prev-and-next-buttons (pagination-prev pagination))
           (who:htm
            (:li :class "page-item"
                 (:a :class "page-link"
                     :href (when href (funcall href (pagination-prev pagination)))
                     :onclick (when on-click (funcall on-click (pagination-prev pagination)))
                     (who:str (who:escape-string "<"))))))
         (dolist (page (pagination-pages pagination))
           (if (eq page :ellipsis)
               (who:htm (:li :class "page-item disabled"
                             (:a :class "page-link" :href "#" (who:str "..."))))
               (who:htm
                (:li :class (concatenate 'string "page-item" (if (= page (pagination-current pagination)) " active" ""))
                     (:a :class "page-link"
                         :href (when href (funcall href page))
                         :onclick (when on-click (funcall on-click page))
                         (who:str page))))))
         (when (and prev-and-next-buttons (pagination-next pagination))
           (who:htm
            (:li :class "page-item"
                 (:a :class "page-link"
                     :href (when href (funcall href (pagination-next pagination)))
                     :onclick (when on-click (funcall on-click (pagination-next pagination)))
                     (who:str (who:escape-string ">"))))))
         (when first-and-last-buttons
           (who:htm
            (:li :class "page-item"
                 (:a :class "page-link"
                     :href (when href (funcall href (pagination-total pagination)))
                     :onclick (when on-click (funcall on-click (pagination-total pagination)))
                     (who:str (who:escape-string ">>"))))))
         )))

;; https://www.w3schools.com/w3css/w3css_pagination.asp
(defun print-pagination-w3css (pagination
                               &key
                                 href on-click
                                 (stream *standard-output*)
                                 (first-and-last-buttons t)
                                 (prev-and-next-buttons t))
  "Like PRINT-PAGINATION-HTML, but for W3CSS framework."
  (who:with-html-output (html stream)
    (:div :class "w3-bar"
          (when first-and-last-buttons
            (who:htm
             (:a :class "w3-button"
                 :href (when href (funcall href 1))
                 :onclick (when on-click (funcall on-click 1))
                 (who:str (who:escape-string "<<")))))
          (when (and prev-and-next-buttons (pagination-prev pagination))
            (who:htm
             (:a :class "w3-button"
                 :href (when href (funcall href (pagination-prev pagination)))
                 :onclick (when on-click (funcall on-click (pagination-prev pagination)))
                 (who:str (who:escape-string "<")))))
          (dolist (page (pagination-pages pagination))
            (if (eq page :ellipsis)
                (who:htm (:a :class "w3-button" :href "#" (who:str "...")))
                (who:htm
                 (:a :class (concatenate 'string "w3-button" (if (= page (pagination-current pagination)) " w3-green" ""))
                     :href (when href (funcall href page))
                     :onclick (when on-click (funcall on-click page))
                     (who:str page)))))
          (when (and prev-and-next-buttons (pagination-next pagination))
            (who:htm
             (:a :class "w3-button"
                 :href (when href (funcall href (pagination-next pagination)))
                 :onclick (when on-click (funcall on-click (pagination-next pagination)))
                 (who:str (who:escape-string ">")))))
          (when first-and-last-buttons
            (who:htm
             (:a :class "w3-button"
                 :href (when href (funcall href (pagination-total pagination)))
                 :onclick (when on-click (funcall on-click (pagination-total pagination)))
                 (who:str (who:escape-string ">>")))))
          )))

(defun page-start-end (page page-size total)
  "Utility function for calculating start and end for PAGE, PAGE-SIZE and TOTAL.
Useful for getting the items of a page, given those arguments and a sequence.

Example usage:
    (apply #'subseq my-seq (multiple-value-list page page-size (length my-seq)))"
  (values (* (1- page) page-size)
          (min (+ (* (1- page) page-size) page-size) total)))

(defun pagination-sample (total &rest args)
  (dotimes (x total)
    (let* ((page (1+ x))
           (pagination (apply #'paginate page total args)))
      (print-pagination pagination)
      (terpri))))

#+test(pagination-sample 30)
#+test(pagination-sample 30 :padding 1)
#+test(pagination-sample 30 :padding 2)
#+test(pagination-sample 30 :padding 3)
#+test(pagination-sample 30 :include-first-and-last nil)
#+test(pagination-sample 30 :include-first-and-last nil :use-ellipsis nil)
