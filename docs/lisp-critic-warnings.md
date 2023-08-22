# lisp-critic-warnings

Signal compiler warnings with lisp-critic critiques.

[[source code]](../lisp-critic-warnings.lisp)

- **Author**: Mariano Montone <marianomontone@gmail.com>
- **Version**: 0.1
- **Requires**: lisp-critic, compiler-hooks, alexandria


 Signal compiler warnings with lisp-critic critiques.

 After the module is loaded, LISP-CRITIC warnings with critiques are signaled on COMPILE-FILE calls.

 Use CRITIQUE declamations to control what packages, files, functions gets critiqued:
     (declaim lisp-critic-warnings:critique nil :package)
     (declaim lisp-critic-warnings:critique nil :file)
     (declaim lisp-critic-warnings:critique nil my-function)



## Variables
### \*critic-warnings\*
Variable for enabling or disabling Lisp critic warnings.

### \*ignore-defs\*
The list of functions to ignore when critiquing.

### \*ignore-files\*
The list of files to ignore when critiquing.

### \*ignore-packages\*
The list of packages to ignore when critiquing.

