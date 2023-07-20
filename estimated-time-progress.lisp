(require :cl-progress-bar)

(defpackage estimated-time-progress
  (:local-nicknames
   (:pb :cl-progress-bar.progress))
  (:use :cl)
  (:export :with-estimated-time-progress))

(in-package :estimated-time-progress)

(defconstant +seconds-in-one-hour+ 3600)
(defconstant +seconds-in-one-minute+ 60)

(defun format-time-in-seconds-minutes-hours (stream in-seconds)
  (when (>= in-seconds +seconds-in-one-hour+)
    (let* ((hours (floor (/ in-seconds +seconds-in-one-hour+))))
      (decf in-seconds (* hours +seconds-in-one-hour+))
      (format stream " ~a hour~p " hours hours)))
  (when (>= in-seconds +seconds-in-one-minute+)
    (let* ((minutes (floor (/ in-seconds +seconds-in-one-minute+))))
      (decf in-seconds (* minutes +seconds-in-one-minute+))
      (format stream "~a minute~p " minutes minutes)))
  (unless (zerop in-seconds)
    (format stream "~d seconds" (truncate in-seconds))))

;; (format-time-in-seconds-minutes-hours t 160)


(defclass estimated-time-progress (pb::progress-bar)
  ()
  (:documentation "Displays progress estimated time for completion."))

(defmethod pb::update-display ((progress-bar estimated-time-progress))
  (incf (pb::progress progress-bar) (pb::pending progress-bar))
  (setf (pb::pending progress-bar) 0)
  (setf (pb::last-update-time progress-bar) (get-internal-real-time))
  (unless (zerop (pb::progress progress-bar))
    (let ((current-time (- (pb::last-update-time progress-bar)
                           (pb::start-time progress-bar))))
      (format t "Progress: ~d% (~a of ~a) at ~f/sec. "
              (truncate
               (* (/ (pb::progress progress-bar)
                     (pb::total progress-bar)) 100))
              (pb::progress progress-bar)
              (pb::total progress-bar)
	      (* (/ (pb::progress progress-bar)
		    current-time)
		 1000000))
      (write-string "Elapsed: ")
      (format-time-in-seconds-minutes-hours t (/ current-time 1000000))
      (write-string ". ")
      (let ((estimated-seconds (/
                                (-
                                 (/ (* (pb::total progress-bar)
                                       current-time)
                                    (pb::progress progress-bar))
                                 current-time)
                                1000000)))
        (write-string "Remaining: ")
        (format-time-in-seconds-minutes-hours t estimated-seconds)
        (write-string "."))
      (terpri)
      (finish-output))))

(defmacro with-estimated-time-progress ((steps-count description &rest desc-args) &body body)
  (let ((!old-bar (gensym)))
    `(let* ((,!old-bar cl-progress-bar::*progress-bar*)
            (cl-progress-bar::*progress-bar* (or ,!old-bar
                                                 (when cl-progress-bar::*progress-bar-enabled*
                                                   (make-instance 'estimated-time-progress :total ,steps-count)))))
       (unless (eq ,!old-bar cl-progress-bar::*progress-bar*)
         (fresh-line)
         (format t ,description ,@desc-args)
         (cl-progress-bar.progress:start-display cl-progress-bar::*progress-bar*))
       (prog1 (progn ,@body)
         (unless (eq ,!old-bar cl-progress-bar::*progress-bar*)
           (cl-progress-bar.progress:finish-display cl-progress-bar::*progress-bar*))))))


;; Usage:

;; (defun perform-step () ; Calls to the update can occur anywhere.
;;   (sleep 1.7)
;;   (cl-progress-bar:update 1))

;; (with-estimated-time-progress (5 "This is just a example. Number of steps is ~a." 5)
;;   (dotimes (i 5) (perform-step)))

(provide :estimated-time-progress)
