(in-package :sgml-composer)

(sgml (:div (:class "foo") "hello"))

(write-sgml (sgml (:div (:class "foo") "hello")))

(write-sgml
 (sgml (:div (:class "foo")
             (:p () "bah")
             "hello")))

(write-sgml
 (sgml (:div (:class "foo")
             (loop for message in '("hello" "cruel" "world")
                   collect
                   (:p (:type message) message)))))

(write-sgml
 (sgml (:div ()
             (when t
               (:span () "Yes"))
             (when nil
               (:span () "No")))))

(write-sgml-indented
 (sgml (:div (:class "foo")
             (loop for message in '("hello" "cruel" "world")
                   collect
                   (:p (:type message) message))))
 *standard-output*)
