(defpackage :clos-star-test
  (:use :cl :clos-star))

(in-package :clos-star-test)

;; examples from https://github.com/lisp-maintainers/defclass-std/

(macroexpand-1
 '(defclass* computer (gadget)
   ((screen :type string)
    (mouse :type string)
    (keyboard :type string)
    (bluetooth :writer bluetooth)
    (touchpad :writer touchpad)
    (speaker :reader speaker)
    (microphone :reader microphone)
    (place :reader computer-place :allocation :class
           :documentation "Where it is"
           :initarg nil)
    (owner :allocation :class :initform "Me"
           :writer :owner
           :initarg nil))
   (:generate (:accessors :prefix computer-) :initargs)))

(macroexpand-1
 '(defclass* language ()
   (name plurals singulars uncountables irregulars)
   (:generate :initargs :initforms :accessors)))
