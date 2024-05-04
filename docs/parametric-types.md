# parametric-types

Some parametric types for CL (an experiment).

[[source code]](../parametric-types.lisp)

- **Author**: Mariano Montone <marianomontone@gmail.com>
- **Version**: 0.1


 Provides some parametric types, like alist-of, list-of and cons-of. This is just an experiment, so don't expect the best of the implementations.

 ## Usage:

 Use the parametric types in your functions:

     (declaim (ftype (function ((alist-of symbol string)) t) foo))




