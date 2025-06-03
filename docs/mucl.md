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
### defpackage

```lisp
(&rest options)
```



### defun

```lisp
(name lambda-list &body body)
```



### destructuring-bind

```lisp
(lambda-list expression &body body)
```



### dolist

```lisp
((var list &optional result) &body body)
```



### lambda

```lisp
(lambda-list &body body)
```



### let\*

```lisp
(bindings &body body)
```



### multiple-value-bind

```lisp
(lambda-list expression &body body)
```



