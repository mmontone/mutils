;;; mupaginator --- A helper package for pagination of collections.
;;
;; https://www.zacfukuda.com/blog/pagination-algorithm
;;
;; Version: 0.1
;; Requires: cl-who
;;
;;; Commentary:
;;
;; Helper package for implementing pagination of collections.
;;
;; Usage:
;;
;;; Code:

(require :cl-who)

(defpackage :mupaginator
  (:use :cl)
  (:export #:paginate
           #:print-pagination
           #:print-pagination-html
           #:print-pagination-bootstrap
           #:print-pagination-w3css
           #:page-start-end))

(in-package :mupaginator)

(defstruct pagination
  current next prev pages total)

(defun paginate (current total &key (use-ellipsis t) (window-size 4) (padding 2))
  (let ((prev (if (= current 1) nil (1- current)))
        (next (if (= current total) nil (1+ current)))
        (pages (list 1)))
    (when (= current total 1)
      (return-from paginate
        (make-pagination :current current
                         :next next
                         :prev prev
                         :pages pages
                         :total total)))
    (when (and use-ellipsis (> current window-size))
      (alexandria:appendf pages (list :ellipsis)))
    (let ((r1 (- current padding))
          (r2 (+ current padding)))
      (loop with i = (if (> r1 padding) r1 padding)
            while (<= i (min total r2))
            do (alexandria:appendf pages (list i))
               (incf i))
      (when (and use-ellipsis (< (+ r2 1) total))
        (alexandria:appendf pages (list :ellipsis)))
      (when (< r2 total)
        (alexandria:appendf pages (list total)))
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
;; (paginate 5 10 :window-size 2 :padding 3)
;; (paginate 5 10 :padding 3 :use-ellipsis nil)

(defun print-pagination (pagination &optional (stream *standard-output*))
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
  (who:with-html-output (html stream)
    (:div :class "pagination"
          (when first-and-last-buttons
            (who:htm
             (:a :class "btn"
                 :href (when href (funcall href 1))
                 :onclick (when on-click (funcall on-click 1))
                 (who:str (who:escape-string "<<")))))
          (when (and prev-and-next-buttons (pagination-prev pagination))
            (who:htm
             (:a :class "btn"
                 :href (when href (funcall href (pagination-prev pagination)))
                 :onclick (when on-click (funcall on-click (pagination-prev pagination)))
                 (who:str (who:escape-string "<")))))
          (dolist (page (pagination-pages pagination))
            (if (eq page :ellipsis)
                (who:htm (:span :class "ellipsis" (who:str "...")))
                (who:htm
                 (:a :class (concatenate 'string "btn" (if (= page (pagination-current pagination)) " btn-primary" ""))
                     :href (when href (funcall href page))
                     :onclick (when on-click (funcall on-click page))))))
          (when (and prev-and-next-buttons (pagination-next pagination))
            (who:htm
             (:a :class "btn"
                 :href (when href (funcall href (pagination-next pagination)))
                 :onclick (when on-click (funcall on-click (pagination-next pagination)))
                 (who:str (who:escape-string ">")))))
          (when first-and-last-buttons
            (who:htm
             (:a :class "btn"
                 :href (when href (funcall href (pagination-total pagination)))
                 :onclick (when on-click (funcall on-click (pagination-total pagination)))
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
  "(apply #'subseq my-seq (multiple-value-list page page-size (length my-seq)))"
  (values (* (1- page) page-size)
          (min (+ (* (1- page) page-size) page-size) total)))