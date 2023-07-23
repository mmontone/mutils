(defpackage :mutils
  (:use :cl)
  (:export
   ;; Utilities
   #:condp
   #:with-output-to-destination
   ;; Modules api
   #:parse-lisp-module-file
   #:list-modules
   #:describe-module
   #:describe-modules))

(in-package :mutils)

(pushnew
 (asdf:system-source-directory :mutils)
 directory-module-loader:*module-directories*)

(defmacro condp (predicate &body clauses)
  "COND using PREDICATE."
  (let ((pred (gensym)))
    `(let ((,pred ,predicate))
       (cond
         ,@(loop for clause in clauses
                 collect `((funcall ,pred ,(car clause))
                           ,@(cdr clause)))))))

(defun call-with-output-to-destination (destination function &rest args)
  "Evaluate FUNCTION with a stream created from DESTINATION as argument.
If DESTINATION is a pathname, then it is opened for writing.
If it is a string, then it is interpreted as a PATHNAME.
If it is a stream, then it is used as it is.
If it is NIL, then WITH-OUTPUT-TO-STRING is used to create the stream.
If it is T, then *STANDARD-OUTPUT* is used for the stream.
ARGS are used for OPEN-FILE calls."
  (etypecase destination
    ((or pathname string)
     (let ((stream (apply #'open destination :direction :output args)))
       (unwind-protect
            (funcall function stream)
         (close stream))))
    (stream
     (funcall function destination))
    (null
     (with-output-to-string (stream)
       (funcall function stream)))
    (t
     (funcall function *standard-output*))))       
    
(defmacro with-output-to-destination ((var destination &rest args) &body body)
  "Evaluate BODY with VAR bound to a stream created from DESTINATION.
If DESTINATION is a pathname, then it is opened for writing.
If it is a string, then it is interpreted as a PATHNAME.
If it is a stream, then it is used as it is.
If it is NIL, then WITH-OUTPUT-TO-STRING is used to create the stream.
If it is T, then *STANDARD-OUTPUT* is used for the stream.
ARGS are used for OPEN-FILE calls."
  `(call-with-output-to-destination ,destination (lambda (,var) ,@body) ,@args))

(defun parse-lisp-module-file (file)
  "Parse a Lisp module file.

Returns the name of the module, its short description,
its properties (author, requirements, keywords, etc.),
its long description/comment with instructions of usage, etc."
  (let ((header-regex "^\\s*;;;\\s*(.*)\\s*---\\s*(.*)")
        (commentary-regex "^\\s*;;;\\s*Commentary\\:")
        (code-regex "^\\s*;;;\\s*Code\\:")
        (property-regex "^\\s*;;\\s*(\\w*)\\:\\s*(.*)")
        (commented-line-regex "^\\s*;*(.*)"))

    (with-open-file (in file)
      ;; The first line has the module name followed by a short description.
      (let* ((first-line (read-line in))
             (module-name-and-desc
               (second (multiple-value-list (ppcre:scan-to-strings header-regex first-line))))
             (module-name nil)
             (short-desc nil)
             (properties nil)
             (commentary nil)
             (status :properties))
        (unless module-name-and-desc
          (error "Module should start with: ;;; <module name> --- <short description>"))
        (setf module-name (string-trim '(#\space) (aref module-name-and-desc 0)))
        (setf short-desc (string-trim '(#\space) (aref module-name-and-desc 1)))
        (handler-case
            (do ((line (read-line in) (read-line in)))
                ((eql status :code) (values))
              (ecase status
                (:properties
                 ;; The following things to expect are the module properties
                 (when (ppcre:scan commentary-regex line)
                   (setf status :commentary))
                 (when (ppcre:scan code-regex line)
                   (error "Commentary section missing."))
                 (let ((property (second (multiple-value-list (ppcre:scan-to-strings property-regex line)))))
                   (when property
                     (push (cons (aref property 0)
                                 (aref property 1))
                           properties))))
                (:commentary
                 (cond
                   ((ppcre:scan code-regex line)
                    (when (null commentary)
                      (error "Commentary is empty"))
                    (setf commentary
                          (with-output-to-string (s)
                            (dolist (line (nreverse commentary))
                              (write-line line s))))
                    (setf status :code))
                   ((ppcre:scan commented-line-regex line)
                    (push (aref (second (multiple-value-list (ppcre:scan-to-strings commented-line-regex line))) 0)
                          commentary))))))
          (end-of-file ()
            (error "End of file when trying to parse module ~a" status)))
        (list :name module-name
              :description short-desc
              :properties properties
              :commentary commentary)))))

(declaim (ftype (function (&optional (member :name :details)) list) list-modules))
(defun list-modules (&optional (return :name))
  "List mutils modules.
RETURN can be:
- :name . Just returns the name of the modules, as a keyword. Default.
- :details. Parses the modules and returns its details."
  (ecase return
    (:name
     (let ((directory-module-loader:*module-directories*
             (list (asdf:system-source-directory :mutils))))
       (sort (remove :mutils (directory-module-loader:list-all-modules))
             #'string< :key #'string)))
    (:details
     (let ((directory-module-loader:*module-directories*
             (list (asdf:system-source-directory :mutils))))
       (sort
        (mapcar #'parse-lisp-module-file
                (remove "mutils"
                        (directory-module-loader:list-all-modules :pathname)
                        :key #'pathname-name
                        :test #'string=))
        #'string<
        :key (alexandria:rcurry #'getf :name))))))

(defun describe-modules ()
  "Print a description of available mutils modules."
  (dolist (module-description (list-modules :details))
    (format t "~a - ~a~%"
            (getf module-description :name)
            (getf module-description :description))))

(declaim (ftype (function ((or symbol string)) (values)) describe-module))
(defun describe-module (module-name)
  "Print a description of module."
  (let ((module (find (string-downcase (string module-name))
                      (list-modules :details)
                      :key (alexandria:rcurry #'getf :name)
                      :test #'string=)))
    (unless module
      (error "Module not found: ~a" module-name))
    (format t "~a~%~%~a"
            (getf module :name)
            (getf module :commentary))))
