;;; directory-module-loader --- Loader of Lisp module files from directories.

;; Copyright (C) 2023 Mariano Montone. All rights reserved.

;; This work is licensed under the terms of the MIT license.  
;; For a copy, see <https://opensource.org/licenses/MIT>.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Version: 0.1
;; Requires: alexandria

;;; Commentary:

;; Loader of Lisp module files from directories.
;; Lisp module files are similar to Emacs packages.
;; requires at the top, and provide at the bottom.
;; and package information in source code comments.

;;; Code:

(defpackage directory-module-loader
  (:use :cl)
  ;; Happily, all those implementations all have the same module-provider hook interface.
  #+(or abcl clasp cmucl clozure ecl mezzano mkcl sbcl)
  (:import-from #+abcl :sys #+(or clasp cmucl ecl) :ext #+clozure :ccl #+mkcl :mk-ext #+sbcl sb-ext #+mezzano :sys.int
                #:*module-provider-functions*
                #+ecl #:*load-hooks*)
  #+(or clasp mkcl) (:import-from :si #:*load-hooks*)
  (:export
   #:*module-directories*
   #:list-all-modules))

(in-package :directory-module-loader)

(defvar *module-directories* nil
  "A list of pathnames (directories) where to look for module files.")

(defun module-provide-directory (module-name)
  (dolist (dir *module-directories*)
    (dolist (lisp-module-file (uiop/filesystem:directory-files dir "*.lisp"))
      (when (equalp (pathname-name lisp-module-file) (string module-name))
        (load lisp-module-file)
        (return-from module-provide-directory lisp-module-file))))
  nil)

#+(or abcl clasp cmucl clozure ecl mezzano mkcl sbcl)
(pushnew 'module-provide-directory *module-provider-functions*)
#+(or clasp mkcl ecl)
(pushnew 'module-provide-directory *load-hooks*)

(declaim (ftype (function (&optional (member :name :pathname)) list)
                list-all-modules))
(defun list-all-modules (&optional (return :name))
  "List all modules available for loading in *MODULE-DIRECTORIES*.
Args:
- RETURN: What the function should return. Either :name or :pathname."
  (let ((modules '()))
    (dolist (dir *module-directories*)
      (dolist (lisp-module-file (uiop/filesystem:directory-files dir "*.lisp"))
        (pushnew
         (case return
           (:name (intern (string-upcase (pathname-name lisp-module-file)) :keyword))
           (:pathname lisp-module-file))
         modules)))
    modules))

(provide :directory-module-loader)
