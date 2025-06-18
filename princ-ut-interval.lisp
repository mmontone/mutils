;;; princ-ut-interval --- Print UNIVERSAL-TIME intervals. 

;; Copyright (C) 2023 Nick Allen. All rights reserved.
;; http://github.com/nallen05/princ-ut-interval

;; Author: Nick Allen
;; Version: 0.1

;;; Commentary:

;; Print UNIVERSAL-TIME intervals.

;; Interaction example:

;;     PRINC-UT-INTERVAL> (princ-ut-interval* (rw-ut:read-time-string "2007/1/3")
;;                                        (rw-ut:read-time-string "2008/3/5")
;;                                        '(:day))
;;
;;     "427 days"
;;
;;     PRINC-UT-INTERVAL> (princ-ut-interval* (rw-ut:read-time-string "2007/1/3")
;;                                        (rw-ut:read-time-string "2008/3/5")
;;                                        '(:week :day))
;;
;;     "61 weeks, 0 days"
;;
;;     PRINC-UT-INTERVAL> (princ-ut-interval* (rw-ut:read-time-string "2007/1/3")
;;                                        (rw-ut:read-time-string "2008/3/5")
;;                                        '(:year :week :day))
;;
;;     "1 year, 8 weeks, 6 days"
;;

;;; Code:
                                        ;
(defpackage :princ-ut-interval
  (:use :cl)
  (:export ; decoding intervals
           :decode-ut-interval
	   :decode-ut-interval*

	   ; extending for different languages
	   :*princ-ut-interval-known-languages*
	   :*princ-ut-interval-known-languages/verbose*
	   :princ-ut-interval-time-unit-name
	   :princ-ut-interval-quantified-vervose-time-unit-name

	   ; using
	   :princ-ut-interval
	   :princ-ut-interval*))

(in-package :princ-ut-interval)	   

; special

(defvar *princ-ut-interval-known-languages* '(:english))
(defvar *princ-ut-interval-known-languages/verbose* '(:english))

(defconstant +second+ 1)
(defconstant +minute+ (* +second+ 60))
(defconstant +hour+ (* +minute+ 60))
(defconstant +day+ (* +hour+ 24))
(defconstant +week+ (* +day+ 7))
(defconstant +month+ (* 30 +day+))
(defconstant +year+ (* 365 +day+))

; DECODE-UT-INTERVAL / DECODE-UT-INTERVAL*

(defvar *unit-seconds-table*
  (list (cons :second +second+)
	(cons :minute +minute+)
	(cons :hour +hour+)
	(cons :day +day+)
	(cons :week +week+)
	(cons :month +month+)
	(cons :year +year+)))

(defun decode-ut-interval (delta pattern)
  (when pattern
    (destructuring-bind (1st . rest) pattern
      (let ((% (rest (assoc 1st *unit-seconds-table* :test 'eq))))
	(multiple-value-bind (x new-delta) (floor delta %)
	  (cons x (decode-ut-interval new-delta rest)))))))

(defun decode-ut-interval* (ut1 ut2 pattern)
  (decode-ut-interval (abs (- ut1 ut2)) pattern))

; extending to use other languages

(defgeneric princ-ut-interval-time-unit-name (unit n language)
  (:documentation
"
   (princ-ut-interval-time-unit-name :month 1 :english)

   -> \"month\"

   (princ-ut-interval-time-unit-name :month 15 :english)

   -> \"months\"
"))

(defmethod princ-ut-interval-time-unit-name (unit n language)
  (declare (ignore n))
  (error "the unit ~S or language ~S is not reckognized by PRINC-UT-INTERVAL-TIME-UNIT-NAME" unit language))

(defun polish-units/singular (english-unit)
  (case english-unit
    (:second "sekunda")
    (:minute "minuta")
    (:hour "godzina")
    (:day "dzień")
    (:week "tydzień")
    (:month "miesiąc")
    (:year "rok")))

(defun polish-units/plural-nominativus (english-unit)
  (case english-unit
    (:second "sekundy")
    (:minute "minuty")
    (:hour "godziny")
    (:day "dni")
    (:week "tygodnie")
    (:month "miesiące")
    (:year "lata")))

(defun polish-units/plural-genetivus (english-unit)
  (case english-unit
    (:second "sekund")
    (:minute "minut")
    (:hour "godzin")
    (:day "dni")
    (:week "tygodni")
    (:month "miesięcy")
    (:year "lat")))

(defmethod princ-ut-interval-time-unit-name (unit (n (eql 1)) (l (eql :polish)))
  (declare (ignore n l))
  (polish-units/singular unit))

(defmethod princ-ut-interval-time-unit-name (unit (n integer) (l (eql :polish)))
  (declare (ignore l))
  (let ((last-digit (mod n 10))) 
    (cond 
      ((or (<= 5 last-digit 9) (= 0 last-digit) (<= 11 n 19)) 
       (polish-units/plural-genetivus unit))
      ((<= 1 4) 
       (polish-units/plural-nominativus unit)))))
; extending to use particular languages -- :ENGLISH

(defmethod princ-ut-interval-time-unit-name (unit n (language (eql :english)))
  (format nil "~A~p" (string-downcase (string unit)) n))

; extending :VERBOSE mode to use particular languages

(defgeneric princ-ut-interval-verbose-quantified-time-unit-name (unit n language)
  (:documentation
"
   (princ-ut-interval-verbose-quantified-time-unit-name :month 1 :english)

   -> \"one month\"

   (princ-ut-interval-verbose-quantified-time-unit-name :month 15 :english)

   -> \"fifteen months\"
"))

(defmethod princ-ut-interval-verbose-quantified-time-unit-name (unit n language)
  (declare (ignore n))
  (error "the unit ~S or language ~S is not reckognized by PRINC-UT-INTERVAL-VERBOSE-QUANTIFIED-TIME-UNIT-NAME" unit language))

; extending :VERBOSE mode to use particular languages/verbose -- :ENGLISH

(defmethod princ-ut-interval-verbose-quantified-time-unit-name (unit n (language (eql :english)))
  (format nil "~r ~A~p" n (string-downcase (string unit)) n))

; PRINC-UT-INTERVAL / PRINC-UT-INTERVAL*

(defun princ-ut-interval (delta pattern
			  &key (language :english)
			       remove-zeros
			       max-depth-after-removing-zeros
			       max-depth-before-removing-zeros
			       verbose
			       (delimit-string ", "))
  (let* ((filled-pattern (mapcar (lambda (unit n)
				    (cons n (if verbose
						(princ-ut-interval-verbose-quantified-time-unit-name unit n language)
						(format nil "~A ~A" n (princ-ut-interval-time-unit-name unit n language)))))
				  pattern
				  (decode-ut-interval delta pattern)))
	 (maybe-removed-zeros (if remove-zeros
				  (delete 0
					  (if max-depth-before-removing-zeros
					      (let ((% (member-if-not 'zerop filled-pattern :key 'first)))
						(subseq % 0 (min (length %) max-depth-before-removing-zeros)))
					      filled-pattern )
					  :key 'first)
				  filled-pattern))
	 (maybe-shortened (if max-depth-after-removing-zeros
			      (if (> (length maybe-removed-zeros) max-depth-after-removing-zeros)
				  (subseq maybe-removed-zeros 0 max-depth-after-removing-zeros)
				  maybe-removed-zeros)
			      maybe-removed-zeros)))
    (apply 'concatenate
	   'string
	   (let ((% (mapcan (lambda (a) (list (rest a) (or delimit-string "")))
			    (or maybe-shortened 
				(last filled-pattern)))))
	     (setf (rest (last % 2)) nil)
	     %))))

(defun princ-ut-interval* (ut1 ut2 pattern
			   &key (language :english)
			        remove-zeros
			        max-depth-after-removing-zeros
			        max-depth-before-removing-zeros
			        verbose
			        (delimit-string ", "))
  (princ-ut-interval (abs (- ut1 ut2))
		     pattern
		     :language language
		     :remove-zeros remove-zeros
		     :max-depth-after-removing-zeros max-depth-after-removing-zeros
		     :max-depth-before-removing-zeros max-depth-before-removing-zeros
		     :verbose verbose
		     :delimit-string delimit-string))

(provide :princ-ut-interval)
