# muprotocols

An implementation of protocols that plays nicely with Common Lisp type system.

[[source code]](../muprotocols.lisp)

- **Author**: Mariano Montone <marianomontone@gmail.com>
- **Version**: 0.1


 An implementation of protocols that plays nicely with Common Lisp type system.

 A protocol is a named set of generic functions that are then implemented by different types.

 The fact that a type implements a certain protocol is reflected on the Lisp type system as `implements` types.

 Here is an example usage:

 Let's define two protocols, one for mutable collections, and another for indexable collections.

 Elements can be added and removed to/from mutable collections.

 Protocols are defined with DEFPROTOCOL, that takes a name and a list of definitions, which follow the syntax of DEFGENERIC:

     (defprotocol mutable
       (add (thing mutable)
            (:documentation "Add THING into MUTABLE."))
       (remove* (thing mutable)
            (:documentation "Remove THING from MUTABLE.")))

 Indexable collections can be accessed at an index:

    (defprotocol indexable
       (at (index indexable)
          (:documentation "Get element of INDEXABLE at INDEX.")))

 Protocols are implemented by a type using IMPLEMENT-PROTOCOL.

 Our list collections are both mutable and indexable:

     (defstruct list-collection
       list)

     (implement-protocol mutable list-collection
        (add (thing (coll list-collection))
          (push thing (list-collection-list coll)))
        (remove* (thing (coll list-collection))
           (setf (list-collection-list coll)
                 (remove thing (list-collection-list coll)))))

     (implement-protocol indexable list-collection
        (at ((index integer) (coll list-collection))
           (nth index (list-collection-list coll))))

 Our set collections are mutable, but not indexable:

     (defstruct set-collection
        set)

     (implement-protocol mutable set-collection
        (add (thing (coll set-collection))
           (pushnew thing (set-collection-set coll)))
        (remove* (thing (coll set-collection))
           (setf (set-collection-set coll)
                 (remove thing (set-collection-set coll)))))

 The implementation of a protocol actually defines CLOS methods that specialize on the type involved:

     (defparameter *list* (make-list-collection))
     (defparameter *set* (make-set-collection))

     (add 1 *list*)
     (add 2 *list*)
     (at 0 *list*) => 1

     (add 1 *set*)
     (add 2 *set*)
     (add 2 *set*)
     (at 0 *set*) => error. Not indexable.

 The implementation of a protocol extends the Lisp type system.
 Instances of the types that implement a protocol satisfy the type `(implements &rest protocols)`.

 We can check that for our example:

     (typep (make-list-collection) '(implements mutable)) => t
     (typep (make-list-collection) '(implements mutable indexable)) => t
     (typep (make-set-collection) '(implements mutable)) => t
     (typep (make-set-collection) '(implements indexable)) => nil

 Now we can use Common Lisp type system to restrict the types of inputs to functions based on protocols by:

 1) Declaring function types at top-level:

     ```
     (declaim (ftype (function ((implements mutable indexable)) t)
                     mutate-indexable-collection))

     (defun mutate-indexable-collection (coll)
        (add "foo" coll)
        (at 0 coll))
     ```
 If we try to compile a function call to MUTATE-INDEXABLE-COLLECTION with something that is not both mutable and indexable:

     (mutate-indexable-collection (make-set-collection))

 we get a compile-time error:

     The value #S(set-collection :set nil) is not of type
      (and
       (satisfies muprotocols-example::implements-mutable-protocol-p)
       (satisfies muprotocols-example::implements-indexable-protocol-p))

 2) Using local function declarations:

     ```
     (defun mutate-indexable-collection-2 (coll)
         (declare (type (implements mutable indexable) coll))
         (add "foo" coll)
         (at 0 coll))
     ```

 3) Check with CHECK-TYPE:

     ```
     (defun mutate-indexable-collection-3 (coll)
        (check-type coll (implements indexable mutable))
        (add "foo" coll)
        (at 0 coll))
     ```



## Macros
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

