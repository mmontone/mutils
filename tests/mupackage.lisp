(defpackage :mupackage-test
  (:use :cl :mupackage))

(in-package :mupackage-test)

(define-package-mixin :my-project-parenscript
    (:import-from #:parenscript
                  #:ps #:chain #:lisp #:new #:create))

(define-package-mixin :my-project-sql
    (:import-from #:sxql
                  #:where #:from))

(define-package package-test-1
      (:use :cl)
  (:mixin :my-project-parenscript))

(define-package package-test-2
      (:use :cl)
  (:mixins :my-project-parenscript
           :my-project-sql))
