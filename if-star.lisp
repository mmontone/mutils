;;; if-star --- The if* macro used in Allegro.
;;
;; This is in the public domain... please feel free to put this definition
;; in your code or distribute it with your version of lisp.
;;
;; Author: John Foderaro
;;
;;; Commentary:
;;
;; The if* macro used in Allegro.
;;
;; Supports the keywords `then`, `else`, `elseif` and `thenret`.
;;
;; Example usage:
;;
;;     (let ((num (random 10)))
;;        (if* (< num 5) then
;;           (format t "~a is less than five" num)
;;          elseif (> num  7) then
;;             (format t "~a is more than seven" num)
;;          else (format t "~a is more or equal to 7" num)))
;;
;;; Code:

(defpackage :if-star
  (:use :cl)
  (:export #:if*))

(in-package :if-star)

(defvar if*-keyword-list '("then" "thenret" "else" "elseif"))

(defmacro if* (&rest args)
  (do ((xx (reverse args) (cdr xx))
       (state :init)
       (elseseen nil)
       (totalcol nil)
       (lookat nil nil)
       (col nil))
      ((null xx)
       (cond ((eq state :compl)
              `(cond ,@totalcol))
             (t (error "if*: illegal form ~s" args))))
    (cond ((and (symbolp (car xx))
                (member (symbol-name (car xx))
                        if*-keyword-list
                        :test #'string-equal))
           (setq lookat (symbol-name (car xx)))))

    (cond ((eq state :init)
           (cond (lookat (cond ((string-equal lookat "thenret")
                                (setq col nil
                                      state :then))
                               (t (error
                                   "if*: bad keyword ~a" lookat))))
                 (t (setq state :col
                          col nil)
                    (push (car xx) col))))
          ((eq state :col)
           (cond (lookat
                  (cond ((string-equal lookat "else")
                         (cond (elseseen
                                (error
                                 "if*: multiples elses")))
                         (setq elseseen t)
                         (setq state :init)
                         (push `(t ,@col) totalcol))
                        ((string-equal lookat "then")
                         (setq state :then))
                        (t (error "if*: bad keyword ~s"
                                  lookat))))
                 (t (push (car xx) col))))
          ((eq state :then)
           (cond (lookat
                  (error
                   "if*: keyword ~s at the wrong place " (car xx)))
                 (t (setq state :compl)
                    (push `(,(car xx) ,@col) totalcol))))
          ((eq state :compl)
           (cond ((not (string-equal lookat "elseif"))
                  (error "if*: missing elseif clause ")))
           (setq state :init)))))

(provide :if-star)
