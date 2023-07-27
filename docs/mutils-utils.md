# mutils-utils

General purpose utilities.

[[source code]](../mutils-utils.lisp)

- **Author**: Mariano Montone <marianomontone@gmail.com>
- **Version**: 0.1
- **Requires**: mutils


 General purpose utilities.



## Macros
### condp

```lisp
(predicate &body clauses)
```

COND using PREDICATE.



This macro allows the implementation of "custom pattern matchers",
with the resulting code being easy to read.

For example:

    (defun object-kind (obj)
      (cond
        ((typep obj 'number) "number")
        ((typep obj 'string) "string")
        ((typep obj 'hashtable) "map")
        ((typep obj 'package) "module")
        ((typep obj 'class) "type")))

can be rewritten as:

    (defun object-kind (obj)
      (condp [(alexandria:curry]((alexandria:curry) #'typep obj)
        ('number "number")
        ('string "string")
        ('boolean "boolean")
        ('hash-table "map")
        ('package "module")
        ('class "type")))

[ALEXANDRIA:CURRY](ALEXANDRIA:CURRY) and [ALEXANDRIA:RCURRY](ALEXANDRIA:RCURRY) are very useful in companion with this macro.

A regular expression matcher:

    (condp [(alexandria:rcurry]((alexandria:rcurry) [#'ppcre:scan](#'ppcre:scan) "my-string")
       ("^foo" :foo)
       ("^bar" :bar)
       ("^my.*" :mine)
       ("^mi.*" :mio))

A string matcher:

    (condp [(alexandria:curry]((alexandria:curry) #'string= "some")
       ("foo" :foo)
       ("bar" :bar)
       ("some" :some))

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
        (let ((x# ,x))
           (+ x# 22))))

Macroexpansion of (auto-gensym-test 44)`:

    (LET ((#:X1 44))
      (+ #:X1 22))

### with-output-to-destination

```lisp
((var destination &rest args) &body body)
```

Evaluate BODY with VAR bound to a stream created from DESTINATION.



- If DESTINATION is a pathname, then open the file for writing. ARGS are used in the OPEN call.
- If it is a string with a fill-pointer, use WITH-OUTPUT-TO-STRING to create a stream for it.
- If it is a stream, then it is used as it is.
- If it is NIL, then WITH-OUTPUT-TO-STRING is used to create the stream.
- If it is T, then *STANDARD-OUTPUT* is used for the stream.

### with-retry-restart

```lisp
((&optional (msg "Retry.") &rest args) &body body)
```

Setup a RETRY restart for evaluating BODY.





