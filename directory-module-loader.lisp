(defpackage directory-module-loader
  (:use :cl)
  ;; Happily, all those implementations all have the same module-provider hook interface.
  #+(or abcl clasp cmucl clozure ecl mezzano mkcl sbcl)
  (:import-from #+abcl :sys #+(or clasp cmucl ecl) :ext #+clozure :ccl #+mkcl :mk-ext #+sbcl sb-ext #+mezzano :sys.int
                #:*module-provider-functions*
                #+ecl #:*load-hooks*)
  #+(or clasp mkcl) (:import-from :si #:*load-hooks*)
  (:export #:*module-directories*))

(in-package :directory-module-loader)

(defvar *module-directories* nil)

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

(provide :directory-module-loader)
