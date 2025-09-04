(defpackage :mupackage-test
  (:use :cl :mupackage))

(in-package :mupackage-test)

(define-package-mixin :my-project-parenscript
    (:import-from #:parenscript
                  #:ps #:chain #:lisp #:new #:create))

(define-package-mixin :my-project-sql
    (:import-from #:sxql
                  #:select
                  #:where
                  #:from))

(define-package package-test-1
      (:use :cl)
  (:mixin :my-project-parenscript))

(define-package package-test-2
      (:use :cl)
  (:mixins :my-project-parenscript
   :my-project-sql))

(define-read-context parenscript
    (ps . parenscript:ps)
  (chain . parenscript:chain))

(with-read-contexts (parenscript)
  (ps (chain foo bar)))

(define-read-context sxql
    (select . sxql:select)
  (from . sxql:from)
  (where . sxql:where))

(with-read-contexts (sxql)
  (select :* (from "lala")
    (where (:= 'x 22))))

(with-mixin :my-project-sql
  (select :* (from "lala")
    (where (:= 'x 22))))
