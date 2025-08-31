# mucl

Upgraded versions of CL definitions with destructuring and ignorable arguments at binding position.

[[source code]](../mucl.lisp)

- **Author**: Mariano Montone <marianomontone@gmail.com>
- **Version**: 0.1


 Upgraded versions of CL definitions with destructuring and ignorable arguments at binding position.

 Usage:

 In your defpackage add a #:shadowing-import-from #:mucl and the list of exported symbols you want to import.
 Like: (:shadowing-import-from #:mucl #:defun #:lambda #:destructuring-bind #:multiple-value-bind).
 Or use MUCL:DEFPACKAGE to define your package.

 Arguments in lambda-lists that start with _ character are declared ignored.
 Example:

    (lambda (_x) ...)

 Destructuring is supported in positional arguments in lambda-lists.
 Example:

     (funcall (lambda ((x . y))
                  (list x y))
             (cons 1 2))



## Macros
### defclass

```lisp
(name direct-superclasses direct-slots &rest options)
```



### defpackage

```lisp
(&rest options)
```



### defun

```lisp
(name lambda-list &body body)
```

Upgraded version of CL:DEFUN that supports ignorable arguments and destructuring in its lambda-list.





### destructuring-bind

```lisp
(lambda-list expression &body body)
```

Upgraded version of CL:DESTRUCTURING-BIND that supports ignorable arguments.





### dolist

```lisp
((var list &optional result) &body body)
```

Upgraded version of CL:DOLIST that supports destructuring at variable binding position.



Example:

    (dolist ((x . y) my-list-of-conses) ...)

### lambda

```lisp
(lambda-list &body body)
```

Upgraded version of CL:LAMBDA that supports ignorable arguments and destructuring in its lambda-list.





### let\*

```lisp
(bindings &body body)
```

Upgraded version of CL:LET* that supports destructuring and multiple-value binds.



Usage:

If more than one variable is used at binding position, then they are bind via MULTIPLE-VALUE-BIND.

Example:

    (let* ((res found-p (gethash :foo my-table))) ...)

If a list is used at binding position, then DESTRUCTURING-BIND is applied.
Example:

    (let* (((x &key z) (list 'x :z 'z))) (list x z))

### multiple-value-bind

```lisp
(lambda-list expression &body body)
```

Upgraded version of CL:MULTIPLE-VALUE-BIND that supports ignorable arguments.





### with-accessors

```lisp
(bindings instance &body body)
```

Upgraded version of CL:WITH-ACCESSORS that supports accessor symbol in bindings.



For example:

    (with-accessors (my-accessor) my-object ...)

expands to:

    (cl:with-accessors ((my-accessor my-accessor)) my-object ...)

