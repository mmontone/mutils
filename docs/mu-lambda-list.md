# mu-lambda-list

Lambda lists with destructuring and ignorable arguments.

[[source code]](../mu-lambda-list.lisp)

- **Author**: Mariano Montone <marianomontone@gmail.com>
- **Version**: 0.1


 Lambda lists with destructuring and ignorable arguments.

 Usage:

 In your defpackage add a #:shadowing-import-from #:mu-lambda-list and the list of exported symbols you want to import.
 Like: (:shadowing-import-from #:mu-lambda-list #:defun #:lambda #:destructuring-bind #:multiple-value-bind).
 Or use MU-LAMBDA-LIST:DEFPACKAGE to define your package.

 Arguments in lambda-lists that start with _ character are declared ignored.
 Example:

    (lambda (_x) ...)

 Destructuring is supported in positional arguments in lambda-lists.
 Example:

     (funcall (lambda ((x . y))
                  (list x y))
             (cons 1 2))

 TODO: potential idea: Add support for &ignore and &ignorable in lambda-lists



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



### multiple-value-bind

```lisp
(lambda-list expression &body body)
```



