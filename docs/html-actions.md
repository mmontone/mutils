# html-actions

Dynamically register functions as HTTP handlers.

[[source code]](../html-actions.lisp)

- **Author**: Mariano Montone <marianomontone@gmail.com>
- **Version**: 0.1
- **Requires**: hunchentoot, uuid


 Dynamically register functions as HTTP handlers.

 Usage:


 TODO:
  - this code could/should be adapted for Clack.
  - change name to http-handler-functions ? http-callbacks ? html-callbacks ?



## Functions
### find-handler-function

```lisp
(id &optional (place *register-place*))
```


### find-handler-function-or-error

```lisp
(id &optional (place *register-place*))
```


### handler-function-url

```lisp
(function-designator &optional (place *register-place*))
```


### register-handler-function

```lisp
(function-designator &optional (place *register-place*))
```

FUNCTION-DESIGNATOR can be either a FUNCTION object or a SYMBOL.
Returns the id of the registered FUNCTION-DESIGNATOR.




## Variables
### \*register-place\*
### \*use-function-names\*
When possible, use function names as ID.

