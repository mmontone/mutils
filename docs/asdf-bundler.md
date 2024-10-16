# asdf-bundler

A module for copying all ASDF system dependencies to a directory.

[[source code]](../asdf-bundler.lisp)

- **Author**: Mariano Montone <marianomontone@gmail.com>
- **Version**: 0.1
- **Requires**: ASDF, ALEXANDRIA


 This module copies all the dependencies of an ASDF system to a directory.
 Then the ASDF and its dependencies can be loaded pointing ASDF to that directory.

 Usage:

 Use `copy-dependencies` to copy dependencies to a directory. Then setup ASDF to load dependencies from that directory:
 ```lisp
 (asdf:initialize-source-registry
  '(:source-registry
    :ignore-inherited-configuration
    (:tree <dependencies-directory>)))
 ```
 `(asdf:operate 'asdf:load-op <my-asdf-system>)`



## Functions
### copy-dependencies

```lisp
(asdf-system target-directory)
```

Copy dependencies of ASDF-SYSTEM to TARGET-DIRECTORY.




