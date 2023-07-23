# def-properties

Portable extractor of information from Common Lisp definitions.

- **Requires**: alexandria
- **Version**: 0.1
- **Author**: Mariano Montone <marianomontone@gmail.com>


 Portable extractor of information from Common Lisp definitions.



## Functions
### asdf-system-packages

```lisp
(system)
```


### class-properties

```lisp
(class-name &optional shallow)
```


### function-properties

```lisp
(symbol &optional shallow)
```


### generic-function-properties

```lisp
(symbol &optional shallow)
```


### list-lambda-list-args

```lisp
(lambda-list)
```

Takes a LAMBDA-LIST and returns the list of all the argument names.




### macro-properties

```lisp
(symbol &optional shallow)
```


### package-properties

```lisp
(package &optional shallow)
```


### parse-docstring

```lisp
(docstring bound-args &key case-sensitive ignore (package *package*))
```

Parse a docstring.
BOUND-ARGS: when parsing a function/macro/generic function docstring, BOUND-ARGS contains the names of the arguments. That means the function arguments are detected by the parser.
CASE-SENSITIVE: when case-sensitive is T, bound arguments are only parsed when in uppercase.
IGNORE: an optional predicate. When ignore is given and invoking it returns T, the current word is not parsed as special symbol.
PACKAGE: the package to use to read the docstring symbols.




### special-operator-properties

```lisp
(symbol &optional shallow)
```


### symbol-class-p

```lisp
(symbol)
```


### symbol-function-p

```lisp
(symbol)
```


### symbol-generic-function-p

```lisp
(symbol)
```


### symbol-kind-p

```lisp
(symbol kind)
```


### symbol-kinds

```lisp
(symbol)
```

Return the kinds of the SYMBOL.




### symbol-macro-p

```lisp
(symbol)
```


### symbol-properties

```lisp
(symbol &optional shallow type)
```

Collects properties about a symbol.
If TYPE is specified, then SYMBOL is treated as the given TYPE (variable, function, package, etc).
If SHALLOW is T, then only fundamental properties are collected.
Returns a list of alists of properties, one alist for each type of definition that SYMBOL is bound to.




### symbol-structure-p

```lisp
(symbol)
```


### symbol-type-p

```lisp
(symbol)
```


### symbol-variable-p

```lisp
(symbol)
```


### type-properties

```lisp
(symbol)
```


### variable-properties

```lisp
(symbol &optional shallow)
```


