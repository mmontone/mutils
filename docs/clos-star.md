# clos-star

Syntax extensions for CLOS.

[[source code]](../clos-star.lisp)

- **Author**: Mariano Montone <marianomontone@gmail.com>
- **Version**: 0.1


 Syntax extensions for CLOS.

 See [clos-star.lisp](../tests/clos-star.lisp) for examples.



## Macros
### defclass\*

```lisp
(name direct-superclasses direct-slots &rest options)
```

A version of DEFCLASS with some syntax extensions.



Extra options in slots:

- :required boolean | string - If the slots is not initialized via its :initarg, an error is signaled. If a string is given, it is used as error message.
- :export :slot | :accessor | :reader | :writer - Specifies what parts of the slot to export. Can be a single value (i.e. :slot), or a list of values (i.e. (:slot :accessor))

Extra options in class:
- :export :class-name | :slots | :accessors | :all . Can be specified as single value, or a list of values (i.e. (:export :class-name :accessors))
- :initialize [qualifier] (&rest initargs &body body). Generates INITIALIZE-INSTANCE method for the class. Object instance is bound to self' variable.
- :print (stream &key identity type). Generates PRINT-OBJECT method for the class.
- :method name [qualifier] (&rest args). Defines a method that uses self' as first argument and specializes it on the class.
- :generate &rest generate-option. With generate-option ::= :initforms | :initargs | :accessors . Generates initforms, initargs or accessors in slots that do not specify one already.

### defgeneric\*

```lisp
(name args &rest options)
```



### define-condition\*

```lisp
(name direct-superclasses direct-slots &rest options)
```



