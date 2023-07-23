# auto-gensym
%Clojure style AUTO-GENSYM macro.

**Version**: 0.1
**Author**: Mariano Montone <marianomontone@gmail.com>


 Clojure style AUTO-GENSYM macro.



# AUTO-GENSYM

nil

## Macros
### with-auto-gensym

```lisp
(body)
```

Replace auto-gensym symbols in BODY.



auto-gensym symbols are those that end with a # character.

Every symbol matching a unique foo# symbol within a syntax quoted form will be replaced with the same generated symbol.

Examples:

(defmacro auto-gensym-test (x)
  (with-auto-gensym
    `(let ((x# ,x))
       (+ x# 22))))

(macroexpand '(auto-gensym-test 44)) =>
(LET ((#:X1 44))
  (+ #:X1 22))

