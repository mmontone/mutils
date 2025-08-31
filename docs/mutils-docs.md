# mutils-docs

Documentation generator for mutils.

[[source code]](../mutils-docs.lisp)

- **Author**: Mariano Montone <marianomontone@gmail.com>
- **Version**: 0.1
- **Requires**: mutils, simple-doc


 Documentation generator for mutils.



## Functions
### generate-docs

```lisp
()
```

Generate library docs.




### generate-module-docs

```lisp
(module-details pathname &optional package-name)
```

Write MODULE-DETAILS docs to PATHNAME.
MODULE-DETAILS is the result of [MUTILS:PARSE-LISP-MODULE-FILE](MUTILS:PARSE-LISP-MODULE-FILE)




