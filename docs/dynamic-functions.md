# dynamic-functions

Implementation of functions dynamically bound.

[[source code]](../dynamic-functions.lisp)

- **Author**: Mariano Montone
- **Version**: 0.1


 Implementation of functions dynamically bound. The current implementation of the function depends on the current context.

 ## Usage

 Define a function as dynamic:

     (defdynfun foo (x)
       x)

 Then change its implementation for a certain context:

     (dflet ((foo (x) (1+ x)))
       (foo))

 ## Example

 Let's define another function BAR that calls the dynamic function FOO defined above:

     (defun bar () (foo 1))

 Note that BAR is not dynamic, but calls FOO which is dynamic.
 Now see result of dynamic function binding:

     (bar) ;; => 1
     (dflet ((foo (x) (1+ x)))
        (bar)) ;; => 2
     (bar) ;; => 1



## Macros
### dflambda

```lisp
(args &body body)
```



### dflet

```lisp
(fbindings &body body)
```



## Variables
### \*dynfun-context\*
