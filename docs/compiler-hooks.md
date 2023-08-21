# compiler-hooks

Provides hooks for Common Lisp compilation api.

[[source code]](../compiler-hooks.lisp)

- **Author**: Mariano Montone <marianomontone@gmail.com>
- **Version**: 0.1
- **Requires**: alexandria, cl-package-locks


 Provides hooks for Common Lisp compilation api.



## Variables
### \*after-compile-file-hooks\*
List of function-designators that are called after COMPILE-FILE.

### \*after-compile-hooks\*
List of function-designators that are called after COMPILE.

### \*before-compile-file-hooks\*
List of function-designators that are called before COMPILE-FILE.

### \*before-compile-hooks\*
List of function-designators that are called before COMPILE.

### \*compiler-hooks-enabled\*
Toggle this variable for enabling or disabling compiler hooks.

