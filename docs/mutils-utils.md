# mutils-utils

General purpose utilities.

[[source code]](../mutils-utils.lisp)

- **Requires**: mutils
- **Version**: 0.1
- **Author**: Mariano Montone <marianomontone@gmail.com>


 General purpose utilities.



## Macros
### condp

```lisp
(predicate &body clauses)
```

COND using PREDICATE.





### with-output-to-destination

```lisp
((var destination &rest args) &body body)
```

Evaluate BODY with VAR bound to a stream created from DESTINATION.
If DESTINATION is a pathname, then open the file for writing. ARGS are used in the OPEN call.
If it is a string with a fill-pointer, use WITH-OUTPUT-TO-STRING to create a stream for it.
If it is a stream, then it is used as it is.
If it is NIL, then WITH-OUTPUT-TO-STRING is used to create the stream.
If it is T, then *STANDARD-OUTPUT* is used for the stream.





