# mulet

Let* with destructruing and multiple value bind.

[[source code]](../mulet.lisp)

- **Author**: Mariano Montone <marianomontone@gmail.com>
- **Version**: 0.1


 Let* with destructruing and multiple value bind.

 Usage:

 If more than one variable is used at binding position, then they are bind via MULTIPLE-VALUE-BIND.
 Example:

     (mulet ((res found-p (gethash :foo my-table))) ...)

 If a list is used at binding position, then DESTRUCTURING-BIND is applied.
 Example:

     (mulet (((x &key z) (list 'x :z 'z))) (list x z))




## Macros
### let\*

```lisp
(bindings &body body)
```



### mulet

```lisp
(bindings &body body)
```



