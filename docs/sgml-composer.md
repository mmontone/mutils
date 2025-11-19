# sgml-composer

A DSL for composing SGML.

[[source code]](../sgml-composer.lisp)

- **Author**: Mariano Montone <marianomontone@gmail.com>
- **Version**: 0.1
- **Requires**: mutils-utils


 A DSL for composing SGML.

 Usage:

 Use SGML macro to create pieces of SGML:
 ```lisp
 (sgml (:a (:href "codeberg.org") "Hello world"))
 ```
 Then write them to a string, either indented or not:
 ```
 (defparameter +link+ (sgml (:a (:href "codeberg.org") "Hello world")))
 (write-sgml +link+) ;; => "<a href=\"codeberg.org\">Hello world</a>"
 ```

 Pieces of sgml can be composed:

 ```lisp
 (defparameter +items+ (loop for i from 1 to 5 collect (sgml (:li () i))))
 (write-sgml (sgml (:ul () +items+))) ;; => "<ul><li>1</li><li>2</li><li>3</li><li>4</li><li>5</li></ul>"
 ```



## Functions
### make-element

```lisp
(tag attributes children)
```


### write-sgml

```lisp
(element &optional destination)
```


### write-sgml-indented

```lisp
(element &optional destination (indent-offset 2))
```


## Macros
### sgml

```lisp
(&body body)
```



## Slot-Accessors
### element-attributes
### (setf element-attributes)
### element-children
### (setf element-children)
### element-tag
### (setf element-tag)
## Classes
### element
