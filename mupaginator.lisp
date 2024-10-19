;;; mupaginator --- A helper package for pagination of collections.
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
  (:export #:make-pagination
           #:print-pagination
           #:print-pagination-html))

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

(defun test1 (&optional (pages 30))
  (dotimes (i pages)
    (format t "Current ~a: " (1+ i))
    (print-pagination (paginate (1+ i) pages))
    (terpri)))

(defun test2 (&optional (pages 30))
  (dotimes (i pages)
    (format t "Current ~a: " (1+ i))
    (print-pagination (paginate (1+ i) pages :padding 3))
    (terpri)))

(defun print-pagination-html (pagination link-renderer
                              &key (stream *standard-output*)
                                (first-and-last-buttons t)
                                (prev-and-next-buttons t))
  (who:with-html-output (html stream)
    (:div :class "pagination"
          (when first-and-last-buttons
            (who:htm
             (:a :class "btn" :href (funcall link-renderer 1)
                 (who:str (who:escape-string "<<")))))
          (when (and prev-and-next-buttons (pagination-prev pagination))
            (who:htm
             (:a :class "btn" :href (funcall link-renderer (pagination-prev pagination))
                 (who:str (who:escape-string "<")))))
          (dolist (page (pagination-pages pagination))
            (if (eq page :ellipsis)
                (who:htm (:span :class "ellipsis" (who:str "...")))
                (who:htm
                 (:a :class (concatenate 'string "btn" (if (= page (pagination-current pagination)) " btn-primary" ""))
                     :href (funcall link-renderer page)))))
          (when (and prev-and-next-buttons (pagination-next pagination))
            (who:htm
             (:a :class "btn" :href (funcall link-renderer (pagination-next pagination))
                 (who:str (who:escape-string ">")))))
          (when first-and-last-buttons
            (who:htm
             (:a :class "btn" :href (funcall link-renderer (pagination-total pagination))
                 (who:str (who:escape-string ">>")))))
          )))

#+example
(with-output-to-string (s)
  (print-pagination-to-html
   (paginate 2 10)
   (lambda (page)
     (format nil "/page/~a" page))
   :stream s))

(provide :mupaginator)
