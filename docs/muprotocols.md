# muprotocols

An implementation of protocols that plays nicely with Common Lisp type system.

[[source code]](../muprotocols.lisp)

- **Author**: Mariano Montone <marianomontone@gmail.com>
- **Version**: 0.1


 An implementation of protocols that plays nicely with Common Lisp type system.

 This is work in progress at the moment.



## Macros
### check-implements

```lisp
(object &rest protocols)
```

Check that OBJECT implements PROTOCOLS.
An ERROR is signaled if not.





### defprotocol

```lisp
(name &body definitions)
```

Define a protocol.



Protocols have a NAME followed by generic function DEFINITIONS.

Syntax:

    defprotocol ::= (name [documentation] definitions*)
    definitions ::= definition*
    definition ::= (function-name gf-lambda-list)

Example:

    (defprotocol indexable
       "Protocol for indexable objects"
       (get-at (index indexable)
         (:documentation "Get element at INDEX from INDEXABLE."))
       (set-at (value index indexable)
         (:documentation "Set element at INDEX from INDEXABLE.")))

Definitions follow the syntax of DEFGENERIC.
It is required that the name of the protocol (`indexable` in the example)
appears in all definitions, in at least one of the arguments, at the positions where the generic functions are passed instances of objects that implement the protocol.

Protocols are implemented by types using IMPLEMENT-PROTOCOL.

### implement-protocol

```lisp
(name type &body implementations)
```

Implement an already defined via DEFPROTOCOL.
TYPE is the name of the type that implements the PROTOCOL.



Syntax:

    defprotocol ::= (name type implementation*)
    implementation := (name specialized-lambda-list [{declaration}* | documentation] {form}*)

Implementations follow the syntax of DEFMETHOD. It is required that the TYPE specializer appears in the same position as in the protocol definition.

Example:

    (implement-protocol indexable array
        (get-at ((index integer) (arr array))
            (aref arr index))
        (set-at (value (index integer) (arr array))
            (setf (aref arr index) value)))

## Generic-Functions
### implements-protocol-p
Tests if OBJECT implements PROTOCOL.

