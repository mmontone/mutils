(asdf:defsystem :mutils
  :description "A collection of Common Lisp modules."
  :version "0.1"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :homepage "https://github.com/mmontone/mutils"
  :components
  ((:file "directory-module-loader")
   (:file "mutils"))
  :depends-on (:cl-ppcre))
