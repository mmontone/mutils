;;; image-dimensions-reader --- Get image dimensions (PNG/JPG) without loading the file.
;;
;; Copyright (C) 2023 Mihai Bazon. All rights reserved.
;;
;; This work is licensed under the terms of the MIT license.
;; For a copy, see <https://opensource.org/licenses/MIT>.
;;
;; Author: Mihai Bazon
;; Version: 0.1
;;
;;; Commentary:
;;
;; Get image dimensions (PNG/JPG) without loading the file
;;
;; See: https://lisperator.net/blog/get-image-dimensions-png-jpg-without-loading-the-file-in-common-lisp/
;;
;;; Code:

(defpackage :image-dimensions-reader
  (:use :cl)
  (:export #:image-size))

(in-package :image-dimensions-reader)

(defgeneric image-size (input)
  (:documentation "Get the sizes of the image designated by INPUT, without loading the file.
INPUT can be a PATHNAME, a STRING or a STREAM.
The size of the image are returned in a list: (width height).")
  (:method ((input pathname))
    (with-open-file (input input :element-type 'unsigned-byte)
      (image-size input)))

  (:method ((input string))
    (with-open-file (input input :element-type 'unsigned-byte)
      (image-size input)))

  (:method ((input stream))
    (labels ((read-num (count &key pos little)
               (when pos (file-position input pos))
               (loop with num = 0
                     for i = 0 then (+ i 8)
                     for j = (* 8 (1- count)) then (- j 8)
                     repeat count
                     do (setf (ldb (byte 8 (if little i j)) num)
                              (read-byte input))
                     finally (return num)))

             (pos (&optional pos)
               (if pos
                   (file-position input pos)
                   (file-position input)))

             (skip (count)
               (pos (+ count (pos))))

             (maybe-png ()
               (ignore-errors
                (file-position input 0)
                (when (and (= #x89 (read-byte input))
                           (= #x50 (read-byte input))
                           (= #x4E (read-byte input))
                           (= #x47 (read-byte input))
                           (= #x0D (read-byte input))
                           (= #x0A (read-byte input))
                           (= #x1A (read-byte input))
                           (= #x0A (read-byte input)))
                  (list (read-num 4 :pos 16) (read-num 4 :pos 20)))))

             (maybe-gif ()
               (ignore-errors
                (file-position input 0)
                (when (and (= #x47 (read-byte input))
                           (= #x49 (read-byte input))
                           (= #x46 (read-byte input))
                           (= #x38 (read-byte input))
                           (let ((b (read-byte input)))
                             (or (= #x37 b)
                                 (= #x39 b)))
                           (= #x61 (read-byte input)))
                  (list (read-num 2 :pos 6 :little t)
                        (read-num 2 :pos 8 :little t)))))

             (tiff-orientation ()
               ;; TIFF starts with two bytes specifying the byte order
               ;; 0x4949 means little-endian.
               (let* ((start-of-tiff (pos))
                      (le (= #x4949 (read-num 2))))
                 ;; two bytes encoding the number 42 follow
                 (when (eq 42 (read-num 2 :little le))
                   ;; four bytes encoding the image file directory offset,
                   ;; relative to start-of-tiff, so jump to that location.
                   (pos (+ start-of-tiff (read-num 4 :little le)))
                   ;; two bytes count the number of directory entries
                   (let ((count-entries (read-num 2 :little le)))
                     (loop repeat count-entries
                           for tag = (read-num 2 :little le)
                           for type = (read-num 2 :little le)
                           for count = (read-num 4 :little le)
                           for value = (read-num 4 :little le)
                           when (eq tag #x0112) do (return value))))))

             (maybe-jpeg ()
               (let (width height orientation)
                 (ignore-errors
                  (file-position input 0)
                  (when (and (= #xFF (read-byte input))
                             (= #xD8 (read-byte input)))
                    (loop do
                      (unless (= #xFF (read-byte input))
                        (return))
                      (let* ((marker (read-byte input))
                             (index (pos))
                             (length (read-num 2)))
                        (case marker
                          ((#xC0 #xC1 #xC2 #xC3 #xC5 #xC6 #xC7 #xC9 #xCA #xCB #xCD #xCE #xCF)
                           (skip 1)
                           (setf height (read-num 2)
                                 width (read-num 2))
                           (return))
                          ((#xE1)
                           (when (= #x45786966 (read-num 4)) ; Exif
                             (skip 2) ; two nulls
                             (setf orientation (tiff-orientation)))))
                        (pos (+ index length))))))
                 (when (and width height)
                   (if (and orientation (or (= 6 orientation)
                                            (= 8 orientation)))
                       (list height width)
                       (list width height)))))

             (maybe-webp ()
               (ignore-errors
                (file-position input 0)
                (when (and (= #x52 (read-byte input)) ; R
                           (= #x49 (read-byte input)) ; I
                           (= #x46 (read-byte input)) ; F
                           (= #x46 (read-byte input)) ; F
                           (skip 4)
                           (= #x57 (read-byte input))  ; W
                           (= #x45 (read-byte input))  ; E
                           (= #x42 (read-byte input))  ; B
                           (= #x50 (read-byte input))) ; P
                  (let ((seq (make-array 4 :element-type 'unsigned-byte)))
                    (when (= (read-sequence seq input)
                             (length seq))
                      (let ((format (map 'string #'code-char seq)))
                        (when (string= format "VP8 ")
                          (return-from maybe-webp (list (read-num 2 :pos 26 :little t)
                                                        (read-num 2 :pos 28 :little t))))
                        (when (string= format "VP8X")
                          (return-from maybe-webp (list (1+ (read-num 3 :pos 24 :little t))
                                                        (1+ (read-num 3 :pos 27 :little t))))))))))))
      (or (maybe-jpeg)
          (maybe-png)
          (maybe-gif)
          (maybe-webp)))))

(provide :image-dimensions-reader)
