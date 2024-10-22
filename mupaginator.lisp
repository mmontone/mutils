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
;;          (pagination (mupaginator:make-pagination :current page
;;                                                   :total total
;;                                                   :page-size 10))
;;          (items (mapcar #'cadr
;;                         (apply #'subseq *countries* (multiple-value-list (mupaginator:pagination-start-end pagination (length *countries*)))))))
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
           #:make-pagination
           #:print-pagination
           #:print-pagination-html
           #:print-pagination-bootstrap
           #:print-pagination-w3css
           #:page-start-end
           #:pagination-current
           #:pagination-next
           #:pagination-prev
           #:pagination-total))

(in-package :mupaginator)

(defstruct pagination
  (current 1 :type integer)
  (total nil :type integer)
  (page-size 10 :type integer))

(defun pagination-prev (pagination)
  (with-slots (current) pagination
    (if (= current 1) nil (1- current))))

(defun pagination-next (pagination)
  (with-slots (current total) pagination
    (if (= current total) nil (1+ current))))

(defun pagination-buttons (pagination &key (padding 2) (use-ellipsis t)
                                        (include-first-and-last t))
  "Return a list of buttons for PAGINATION.
Args:
- USE-ELLIPSIS: an :ELLIPSIS keyword is included when appropiate when enabled.
- PADDING: the list of buttons has length (PADDING * 2) + 1.
- INCLUDE-FIRST-AND-LAST: buttons for first and last page are included."
  (with-accessors ((current pagination-current)
                   (total pagination-total)
                   (prev pagination-prev)
                   (next pagination-next))
      pagination

    (when (= current total 1)
      (return-from pagination-buttons '(1)))

    (let* ((buttons '())
           (range (cond
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
            (alexandria:appendf buttons '(1)))
          (alexandria:appendf buttons (list :ellipsis))))

      ;; buttons slice
      (loop for i from r1 to r2
            do (alexandria:appendf buttons (list i)))

      (when (< r2 total)
        (when use-ellipsis
          (alexandria:appendf buttons (list :ellipsis))

          ;; last page
          (when include-first-and-last
            (alexandria:appendf buttons (list total)))))

      (assert (member current buttons))
      buttons)))

(defun print-pagination (pagination &optional (stream *standard-output*) &rest options)
  "Debug function for printing a text representation of PAGINATION."
  (dolist (button (apply #'pagination-buttons pagination options))
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
                                (prev-and-next-buttons t)
                                (use-ellipsis t)
                                (padding 2))
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
          (dolist (button (pagination-buttons pagination
                                              :padding padding
                                              :use-ellipsis use-ellipsis
                                              :include-first-and-last first-and-last-buttons))
            (if (eq button :ellipsis)
                (who:htm (:span :class "ellipsis" (who:str "...")))
                (who:htm
                 (:a :class (when (= button (pagination-current pagination)) "active")
                     :href (when href (funcall href button))
                     :onclick (when on-click (funcall on-click button))
                     (who:str button)))))
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
  (print-pagination-html
   (make-pagination :current 2 :total 10)
   :href (lambda (page)
           (format nil "/page/~a" page))
   :stream s))

(defun print-pagination-bootstrap (pagination
                                   &key
                                     href on-click
                                     (stream *standard-output*)
                                     (first-and-last-buttons t)
                                     (prev-and-next-buttons t)
                                     (use-ellipsis t)
                                     (padding 2))
  "Like PRINT-PAGINATION-HTML, but for Bootstrap framework.

See: https://getbootstrap.com/docs/4.1/components/pagination/"
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
         (dolist (button (pagination-buttons pagination :padding padding
                                                        :use-ellipsis use-ellipsis
                                                        :include-first-and-last first-and-last-buttons))
           (if (eq button :ellipsis)
               (who:htm (:li :class "page-item disabled"
                             (:a :class "page-link" :href "#" (who:str "..."))))
               (who:htm
                (:li :class (concatenate 'string "page-item" (if (= button (pagination-current pagination)) " active" ""))
                     (:a :class "page-link"
                         :href (when href (funcall href button))
                         :onclick (when on-click (funcall on-click button))
                         (who:str button))))))
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

(defun print-pagination-w3css (pagination
                               &key
                                 href on-click
                                 (stream *standard-output*)
                                 (first-and-last-buttons t)
                                 (prev-and-next-buttons t)
                                 (use-ellipsis t)
                                 (padding 2))
  "Like PRINT-PAGINATION-HTML, but for W3CSS framework.

See: https://www.w3schools.com/w3css/w3css_pagination.asp"
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
          (dolist (button (pagination-buttons pagination :padding padding
                                                         :use-ellipsis use-ellipsis
                                                         :include-first-and-last first-and-last-buttons))
            (if (eq button :ellipsis)
                (who:htm (:a :class "w3-button" :href "#" (who:str "...")))
                (who:htm
                 (:a :class (concatenate 'string "w3-button" (if (= button (pagination-current pagination)) " w3-green" ""))
                     :href (when href (funcall href button))
                     :onclick (when on-click (funcall on-click button))
                     (who:str button)))))
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

(defun pagination-start-end (pagination &optional length)
  "Utility function for calculating start and end for PAGINATION.
Useful for getting the items of a page for a sequence.

Example usage:
    (let ((pagination (make-pagination :current 2 :total 20 :page-size 10)))
            (apply #'subseq my-seq (multiple-value-list (pagination-start-end pagination))))

See: PAGINATION-SUBSEQ"
  (with-slots (page-size current) pagination
    (values (* (1- current) page-size)
            (if length
                (min (+ (* (1- current) page-size) page-size) length)
                (+ (* (1- current) page-size) page-size)))))

;; (pagination-start-end (make-pagination :current 2 :total 20 :page-size 10) 100)
;; (pagination-start-end (make-pagination :current 3 :total 20 :page-size 10) 100)

(defun pagination-subseq (pagination sequence)
  "Get current page from SEQUENCE, according to PAGINATION."
  (apply #'subseq sequence (multiple-value-list (pagination-start-end pagination (length sequence)))))

#+example
(let* ((p (make-pagination-for-sequence (list-all-packages))))
  (pagination-subseq p (list-all-packages)))

(defun make-pagination-for-sequence (sequence &key (current 1) (page-size 10))
  (make-pagination :current current
                   :page-size page-size
                   :total (truncate (/ (length sequence) page-size))))

(defun pagination-sample (total &rest args)
  (dotimes (x total)
    (let* ((page (1+ x))
           (pagination (make-pagination :current page
                                        :total total)))
      (apply #'print-pagination pagination *standard-output* args)
      (terpri))))

#+test(pagination-sample 30)
#+test(pagination-sample 30 :padding 1)
#+test(pagination-sample 30 :padding 2)
#+test(pagination-sample 30 :padding 3)
#+test(pagination-sample 30 :include-first-and-last nil)
#+test(pagination-sample 30 :include-first-and-last nil :use-ellipsis nil)
