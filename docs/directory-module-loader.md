# directory-module-loader

Loader of Lisp module files from directories.

[[source code]](../directory-module-loader.lisp)

- **Author**: Mariano Montone <marianomontone@gmail.com>
- **Version**: 0.1
- **Requires**: alexandria


 Loader of Lisp module files from directories.
 Lisp module files are similar to Emacs packages.
 requires at the top, and provide at the bottom.
 and package information in source code comments.



## Functions
### list-all-modules

```lisp
(&optional (return :name))
```

List all modules available for loading in *MODULE-DIRECTORIES*.

- **RETURN**: What the function should return. Either :name or :pathname.



## Variables
### \*module-directories\*
A list of pathnames (directories) where to look for module files.

