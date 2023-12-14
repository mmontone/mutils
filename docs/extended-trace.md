# extended-trace

A TRACE replacement with some extra report options.

[[source code]](../extended-trace.lisp)

- **Author**: Mariano Montone <marianomontone@gmail.com>
- **Version**: 0.1
- **Requires**: trivial-backtrace, alexandria, bordeaux-threads


 A TRACE replacement with some extra report options.

 NOTE: this module is WIP and very lacking.




## Macros
### trace

```lisp
(func &rest specs)
```

Extended trace.



Additional :REPORT options:

- :BACKTRACE: Outputs backtrace. SBCL only.
- :THREAD: Outputs current thread. SBCL only.

