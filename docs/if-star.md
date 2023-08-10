# if-star

The if* macro used in Allegro.

[[source code]](../if-star.lisp)

- **Author**: John Foderaro


 The if* macro used in Allegro.

 Supports the keywords `then`, `else`, `elseif` and `thenret`.

 Example usage:

     (let ((num (random 10)))
        (if* (< num 5) then
           (format t "~a is less than five" num)
          elseif (> num  7) then
             (format t "~a is more than seven" num)
          else (format t "~a is more or equal to 7" num)))



## Macros
### if\*

```lisp
(&rest args)
```



