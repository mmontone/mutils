# dynamic-context

Dynamic bindings without the need of defining new dynamic variables.

[[source code]](../dynamic-context.lisp)

- **Author**: Mariano Montone
- **Version**: 0.1


 Dynamic bindings without the need of defining new dynamic variables.
 Dynamic bindings are bound to the context without declaration.



## Functions
### call-with-context

```lisp
(context func)
```


### get-context\*

```lisp
(name &optional default)
```


## Macros
### get-context

```lisp
(name &optional default)
```



### let-context

```lisp
(bindings &body body)
```



### with-context

```lisp
(context &body body)
```



### with-context-bindings

```lisp
(bindings &body body)
```



## Variables
### \*context\*
