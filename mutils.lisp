(defpackage :mutils
  (:use :cl)
  (:export
   #:condp
   #:parse-lisp-module-file
   #:list-modules))

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
        (do ((line (read-line in nil) (read-line in nil)))
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
               ((ppcre:scan commented-line-regex line)
                (push (aref (second (multiple-value-list (ppcre:scan-to-strings commented-line-regex line))) 0)
                      commentary))
               ((ppcre:scan code-regex line)
                (when (null commentary)
                  (error "Commentary is empty"))
                (setf commentary
                      (with-output-to-string (s)
                        (dolist (line (nreverse commentary))
                          (write-line line s))))                          
                (setf status :code))))))
        (list :name module-name
              :description short-desc
              :properties properties
              :commentary commentary)))))

(defun list-modules ()
  (let ((directory-module-loader:*module-directories*
          (list (asdf:system-source-directory :mutils))))
    (remove :mutils (directory-module-loader:list-all-modules))))
