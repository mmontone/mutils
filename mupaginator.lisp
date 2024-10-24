;;; mupaginator --- A helper package for pagination of collections.
;;
;;
;; Version: 0.1
;; Requires: cl-who, alexandria, trivial-types
;;
;;; Commentary:
;;
;; Helper package for implementing pagination of collections.
;;
;; Usage:
;;
;; Create a PAGINATION object using MAKE-PAGINATION function, passing the current page and a source for the pagination, either a SEQUENCE or a FUNCTION-DESIGNATOR that takes a page number and returns two values: the items of that page, and the total number of items.
;; Then print that object to an HTML stream, using of the PRINT-PAGINATION functions, and passing HREF or ON-CLICK handlers for resolving urls/actions for the page buttons.
;;
;; ```lisp
;; (defroute vanilla-pagination-test "/vanilla"
;;     ((page :parameter-type 'integer :init-form 1))
;;   (let ((pagination (mupaginator:make-pagination :current page :source (list-all-packages))))
;;     (with-html
;;       (:ul
;;        (dolist (item (mupaginator:pagination-current-items pagination))
;;          (who:htm (:li (who:str (package-name item))))))
;;       (mupaginator:print-pagination-bootstrap
;;        pagination
;;        :href (lambda (page-nr)
;;                (easy-routes:genurl 'vanilla-pagination-test :page page-nr))
;;        :stream html))))
;; ```
;;
;;; Code:

(require :cl-who)
(require :alexandria)
(require :trivial-types)

(defpackage :mupaginator
  (:use :cl)
  (:export
   ;; construction
   #:pagination
   #:make-pagination
   ;; accessing
   #:pagination-current-items
   #:pagination-current
   #:pagination-next
   #:pagination-prev
   #:pagination-total
   ;; printing
   #:print-pagination
   #:print-pagination-html
   #:print-pagination-bootstrap
   #:print-pagination-w3css))

(in-package :mupaginator)

(defstruct pagination
  (current 1 :type integer)
  (page-size 10 :type integer)
  ;; SOURCE is either a function designator or a sequence.
  ;; When source is a function designator, then it is called with a page number
  ;; and is expected to return two values: the items of that page, and the total number of items
  (source nil :type (or trivial-types:function-designator sequence)))

(defun pagination-prev (pagination)
  (with-slots (current) pagination
    (if (= current 1) nil (1- current))))

(defun pagination-next (pagination)
  (with-accessors ((current pagination-current)
                   (total pagination-total))
      pagination
    (if (= current total) nil (1+ current))))

(defun pagination-current-items (pagination)
  "Returns PAGINATION current page items and total pages."
  (etypecase (pagination-source pagination)
    (trivial-types:function-designator
     (multiple-value-bind (items total)
         (funcall (pagination-source pagination) (pagination-current pagination))
       (assert (integerp total) nil "Second value returned by pagination-source of ~s is expected to be the total number of items, but ~s was returned." pagination total)
       (values items (truncate (/ total (pagination-page-size pagination))))))
    (sequence
     (values
      (let ((start (* (1- (pagination-current pagination))
                      (pagination-page-size pagination))))
        (subseq (pagination-source pagination)
                start
                (min (+ start (pagination-page-size pagination))
                     (length (pagination-source pagination)))))
      (truncate (/ (length (pagination-source pagination))
                   (pagination-page-size pagination)))))))

(defun pagination-total (pagination)
  (cadr (multiple-value-list (pagination-current-items pagination))))

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
                     (cons 1 (min (+ padding padding 1) total)))
                    ((> (+ current padding) total)
                     (cons (max (- total padding padding) 1) total))
                    (t
                     (cons (max (- current padding) 1)
                           (min (+ current padding) total)))))
           (r1 (car range))
           (r2 (cdr range)))

      ;; first page
      (when (> r1 1)
        (when use-ellipsis
          (when include-first-and-last
            (alexandria:appendf buttons '(1)))
          (when (> r1 2)
            (alexandria:appendf buttons (list :ellipsis)))))

      ;; buttons slice
      (loop for i from r1 to r2
            do (alexandria:appendf buttons (list i)))

      (when (< r2 total)
        (when use-ellipsis
          (when (< r2 (1- total))
            (alexandria:appendf buttons (list :ellipsis)))

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
   (make-pagination :current 2 :source (lambda (page)
                                         (values nil 50)))
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

(defun pagination-sample (total &rest args)
  (dotimes (x total)
    (let* ((page (1+ x))
           (pagination (make-pagination :current page
                                        :source (lambda (page)
                                                  (declare (ignore page))
                                                  (values nil (* total 10))))))
      (apply #'print-pagination pagination *standard-output* args)
      (terpri))))

#+test(pagination-sample 30)
#+test(pagination-sample 30 :padding 1)
#+test(pagination-sample 30 :padding 2)
#+test(pagination-sample 30 :padding 3)
#+test(pagination-sample 30 :include-first-and-last nil)
#+test(pagination-sample 30 :include-first-and-last nil :use-ellipsis nil)
