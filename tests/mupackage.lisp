(defpackage :mupackage-test
  (:use :cl :mupackage))

(in-package :mupackage-test)

(define-package-mixin :my-project-parenscript
    (:import-from #:parenscript
                  #:ps #:chain #:lisp #:new #:create))

(define-package package-test-1
      (:use :cl)
  (:mixin :my-project-parenscript))
