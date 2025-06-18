# princ-ut-interval

Print UNIVERSAL-TIME intervals.

[[source code]](../princ-ut-interval.lisp)

- **http**: //github.com/nallen05/princ-ut-interval
- **Author**: Nick Allen
- **Version**: 0.1


 Print UNIVERSAL-TIME intervals.

 Interaction example:

     PRINC-UT-INTERVAL> (princ-ut-interval* (rw-ut:read-time-string "2007/1/3")
                                        (rw-ut:read-time-string "2008/3/5")
                                        '(:day))

     "427 days"

     PRINC-UT-INTERVAL> (princ-ut-interval* (rw-ut:read-time-string "2007/1/3")
                                        (rw-ut:read-time-string "2008/3/5")
                                        '(:week :day))

     "61 weeks, 0 days"

     PRINC-UT-INTERVAL> (princ-ut-interval* (rw-ut:read-time-string "2007/1/3")
                                        (rw-ut:read-time-string "2008/3/5")
                                        '(:year :week :day))

     "1 year, 8 weeks, 6 days"




## Functions
### decode-ut-interval

```lisp
(delta pattern)
```


### decode-ut-interval\*

```lisp
(ut1 ut2 pattern)
```


### princ-ut-interval

```lisp
(delta pattern &key (language :english) remove-zeros
 max-depth-after-removing-zeros max-depth-before-removing-zeros verbose
 (delimit-string ", "))
```


### princ-ut-interval\*

```lisp
(ut1 ut2 pattern &key (language :english) remove-zeros
 max-depth-after-removing-zeros max-depth-before-removing-zeros verbose
 (delimit-string ", "))
```


## Generic-Functions
### princ-ut-interval-time-unit-name

```lisp
(unit n language)
```

(princ-ut-interval-time-unit-name :month 1 :english)



-> "month"

   (princ-ut-interval-time-unit-name :month 15 :english)

   -> "months"
## Variables
### \*princ-ut-interval-known-languages\*
### \*princ-ut-interval-known-languages/verbose\*
