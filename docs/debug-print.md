# debug-print

A reader macro package for debug printing.

[[source code]](../debug-print.lisp)

- **Author**: Satoshi Imai
- **Version**: 0.2
- **Requires**: cl-syntax, named-readtables


 A reader macro package for debug printing.

 ## Usage

    (debug-print:use-debug-print)

 ### Debug print

    (defun fact (n)
      (if (= #>n 0)
          1
          (* n #>(fact (1- n)))))

    (fact 10)

    ;; N => 10
    ;; N => 9
    ;; N => 8
    ;; N => 7
    ;; N => 6
    ;; N => 5
    ;; N => 4
    ;; N => 3
    ;; N => 2
    ;; N => 1
    ;; N => 0
    ;; (FACT (1- N)) => 1
    ;; (FACT (1- N)) => 1
    ;; (FACT (1- N)) => 2
    ;; (FACT (1- N)) => 6
    ;; (FACT (1- N)) => 24
    ;; (FACT (1- N)) => 120
    ;; (FACT (1- N)) => 720
    ;; (FACT (1- N)) => 5040
    ;; (FACT (1- N)) => 40320
    ;; (FACT (1- N)) => 362880

 ### Debug push

    (defun fact2 (n)
      (if (= n 0)
          1
          (* n #!(fact2 (1- n)))))

    (fact2 10)
    ;; => 3628800

 ### Debug push results are stored to *dbg*

    debug-print:*dbg*
    ;; => (362880 40320 5040 720 120 24 6 2 1 1)

    ;;; Clearing debug push list

    (debug-print:clear-dbg)

    debug-print:*dbg*
    ;; => nil

 ## Configure variables

 ### Setting destination stream (default value is *standard-output*)

    (setf debug-print:*destination* *error-output*)

 ### Setting for using DESCRIBE instead of mere value (default value is nil)

    (setf debug-print:*use-describe* t)

    #>'sin

    ;; 'SIN => COMMON-LISP:SIN
    ;;   [symbol]
    ;;
    ;; SIN names a compiled function:
    ;;   Lambda-list: (NUMBER)
    ;;   Declared type: (FUNCTION (NUMBER)
    ;;                   (VALUES
    ;;                    (OR (SINGLE-FLOAT -1.0 1.0)
    ;;                        (DOUBLE-FLOAT -1.0d0 1.0d0)
    ;;                        (COMPLEX SINGLE-FLOAT) (COMPLEX DOUBLE-FLOAT))
    ;;                    &OPTIONAL))
    ;;   Derived type: (FUNCTION (T)
    ;;                  (VALUES
    ;;                   (OR (SINGLE-FLOAT -1.0 1.0)
    ;;                       (DOUBLE-FLOAT -1.0d0 1.0d0)
    ;;                       (COMPLEX DOUBLE-FLOAT) (COMPLEX SINGLE-FLOAT))
    ;;                   &OPTIONAL))
    ;;   Documentation:
    ;;     Return the sine of NUMBER.
    ;;   Known attributes: foldable, flushable, unsafely-flushable, movable, recursive
    ;;   Source file: SYS:SRC;CODE;IRRAT.LISP



## Functions
### clear-dbg

```lisp
nil
```


### debug-print

```lisp
(pre-exp exp)
```


### debug-print-reader

```lisp
(stream char1 char2)
```


### debug-push

```lisp
(obj)
```


### debug-push-reader

```lisp
(stream char1 char2)
```


### use-debug-print

```lisp
nil
```


## Variables
### \*dbg\*
\*DBG\* is a list, and using DEBUG-PUSH(\#!) will be pushed to this list.
To clear this variable, use the CLEAR-DBG function.

### \*destination\*
\*DESTINATION\* allows to configure destination stream for debug prints.

### \*use-describe\*
When \*USE-DESCRIBE\* is true, describe is used for debug prints.

### debug-print-syntax
nil

