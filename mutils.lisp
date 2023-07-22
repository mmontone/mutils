(defpackage :mutils
  (:use :cl)
  (:export
   #:condp
   #:parse-lisp-module-file
   #:list-modules
   #:generate-readme))

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
        (setf module-name (aref module-name-and-desc 0))
        (setf short-desc (aref module-name-and-desc 1))
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
       (remove :mutils (directory-module-loader:list-all-modules))))
    (:details
     (let ((directory-module-loader:*module-directories*
             (list (asdf:system-source-directory :mutils))))
       (mapcar #'parse-lisp-module-file
               (remove "mutils"
                       (directory-module-loader:list-all-modules :pathname)
                       :key #'pathname-name
                       :test #'string=))))))

(defun generate-readme ()
  "Generate a README file with information about available modules."
  (let ((output-file (asdf:system-relative-pathname :mutils "README.md")))
    (with-open-file (f output-file :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create
                                   :external-format :utf-8)
      (write-string (alexandria:read-file-into-string (asdf:system-relative-pathname :mutils "README.base.md")) f)
      (terpri f) (terpri f)
      (write-line "## Modules" f)
      (terpri f)
      (dolist (module-details (list-modules :details))
        (format f "### ~a ~%~%" (getf module-details :name))
        (write-string (getf module-details :commentary) f)
        (terpri f) (terpri f)))))
