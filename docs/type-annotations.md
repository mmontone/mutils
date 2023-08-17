# type-annotations

Support for inline type annotations.

[[source code]](../type-annotations.lisp)

- **Author**: Mariano Montone <marianomontone@gmail.com>
- **Version**: 0.1


 Support for inline type annotations.
 Versions of CL definitions with support for inline type annotations are provided.
 Typed DEFUN, DEFVAR, DEFPARAMETER, etc.

 Type annotations appear after variables and arguments names, and are enclosed between sharp brackets, like: `<type-name>`.

 Usage:

 Use the inline type version of the CL equivalent definer, and inline type annotations:

     (<t>:defun sum (x <integer> y <integer>) <integer>
        (+ x y))

 Annotated definitions are macro-expanded to top-level type declamations:

    (PROGN
        (DECLAIM (FTYPE (FUNCTION (INTEGER INTEGER) INTEGER) SUM))
        (COMMON-LISP:DEFUN SUM (X Y) (+ X Y)))



## Macros
### defparameter

```lisp
(name &rest init)
```

Type annotated DEFPARAMETER.



A type annotation is accepted after the variable name.

Example:

    (<t>:defparameter *my-var* <integer> 22)

The macro is expanded to a top-level type declamation plus a normal DEFPARAMETER:

    (PROGN (DECLAIM (TYPE INTEGER *MY-VAR*))
           [(COMMON-LISP:DEFPARAMETER]((COMMON-LISP:DEFPARAMETER) *MY-VAR* 22))

### defun

```lisp
(name args &body body)
```

Typed annotated DEFUN.



Type annotations can appear after argument names.
Annotations on all required, optional and key arguments are supported.
Also, an optional return type annotation can appear after the function arguments list.

Example:

    (<t>:defun sum (x <integer> y <integer>) <integer>
        (+ x y))

The macro is expanded to a top-level type declamation plus a normal DEFUN:

    (PROGN
         (DECLAIM (FTYPE (FUNCTION (INTEGER INTEGER) INTEGER) SUM))
         [(COMMON-LISP:DEFUN]((COMMON-LISP:DEFUN) SUM (X Y) (+ X Y)))

### defvar

```lisp
(name &rest init)
```

Type annotated DEFVAR.



A type annotation is accepted after the variable name.

Example:

    (<t>:defvar *my-var* <integer> 22)

The macro is expanded to a top-level type declamation plus a normal DEFVAR:

    (PROGN (DECLAIM (TYPE INTEGER *MY-VAR*))
           [(COMMON-LISP:DEFVAR]((COMMON-LISP:DEFVAR) *MY-VAR* 22))

