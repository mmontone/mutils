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

;; https://github.com/AccelerationNet/cl-inflector/blob/master/langs.lisp%5D%5Bcl-inflector
(macroexpand-1
 '(defclass* language ()
   (name plurals singulars uncountables irregulars)
   (:generate :initargs :initforms :accessors)))

;; http://common-lisp.net/project/defclass-star/configuration.lisp.html
(macroexpand-1
 '(defclass* configuration ()
   ((package-name      :type symbol)
    (package-nicknames :initform '())
    (included-files    :initform '())
    (gccxml-path       :initform "gccxml")
    (gccxml-flags      :initform "")
    (hidden-symbols    :initform '())
    (output-filename   :initform nil)
    (options           :initform (standard-configuration-options))
    (symbol-export-filter :initform 'standard-symbol-export-filter
                          :type (or (function (symbol)) symbol))
    (function-name-transformer :initform 'standard-name-transformer
                               :type (or (function (string)) symbol))
    (variable-name-transformer :initform 'standard-name-transformer
                               :type (or (function (string)) symbol))
    (type-name-transformer :initform 'standard-name-transformer
                           :type (or (function (string)) symbol))
    (temp-directory    :initform (make-pathname :directory "/tmp"))
    (working-directory :initform *default-pathname-defaults*))
   (:generate (:accessors :suffix -of) :initargs)))

;; https://github.com/jd/cl-hue/blob/master/cl-hue.lisp

(macroexpand-1
 '(defclass* light ()
   (bridge number type name modelid uniqueid swversion pointsymbol
    (on :accessor light-on-p)
    brightness
    hue saturation xy ct alert effect colormode
    (reachable :accessor light-reachable-p))
   (:generate :initargs (:accessors :prefix light-))))
