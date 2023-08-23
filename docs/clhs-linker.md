# clhs-linker

Replace Lisp terms in a file by hyperlinks to Common Lisp HyperSpec.

[[source code]](../clhs-linker.lisp)

- **Author**: Mariano Montone <marianomontone@gmail.com>
- **Version**: 0.1
- **Requires**: mutils-utils, colorize


 Replace Lisp terms in a file by hyperlinks to Common Lisp HyperSpec.

 Supported input file types are Markdown and HTML.



## Functions
### link-file

```lisp
(pathname &optional (destination *standard-output*))
```

Replace Lisp terms in PATHNAME by links to Common Lisp HyperSpec.



DESTINATION can be: a PATHNAME, a STRING with a FILL-POINTER, a STREAM,
NIL (writes to a string), or T (writes to *STANDARD-OUTPUT*)
.
Supported input file types are Markdown and HTML.
## Variables
### \*lisp-term-regex\*
The regex for matching Lisp terms.

