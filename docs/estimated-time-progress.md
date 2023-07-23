# estimated-time-progress

Progress display with estimated time.

[[source code]](../estimated-time-progress.lisp)

- **Requires**: cl-progress-bar
- **Version**: 0.1
- **Author**: Mariano Montone <marianomontone@gmail.com>


 Progress display with estimated time.

 Usage:

 Enable the progress bars:

 (setf cl-progress-bar:*progress-bar-enabled* t)

 (defun perform-step () ; Calls to the update can occur anywhere.
   (sleep 1.7)
   (cl-progress-bar:update 1))

 (with-estimated-time-progress (5 "This is just a example. Number of steps is ~a." 5)
   (dotimes (i 5) (perform-step)))

 Example output:

 This is just a example. Number of steps is 5.
 Progress: 20% (1 of 5) at 0.5707746/sec. Elapsed: 1 seconds. Remaining: 7 seconds.
 Progress: 40% (2 of 5) at 0.5787024/sec. Elapsed: 3 seconds. Remaining: 5 seconds.
 Progress: 60% (3 of 5) at 0.5804938/sec. Elapsed: 5 seconds. Remaining: 3 seconds.
 Progress: 80% (4 of 5) at 0.58207065/sec. Elapsed: 6 seconds. Remaining: 1 seconds.
 Progress: 100% (5 of 5) at 0.58220625/sec. Elapsed: 8 seconds. Remaining: .

 Finished in 8.60 seconds



## Macros
### with-estimated-time-progress

```lisp
((steps-count description &rest desc-args) &body body)
```

Display progress with estimated times.

- **STEPS-COUNT**: The total number of steps.
- **DESCRIPTION**: A FORMAT control string for the progress display.
- **DESC-ARGS**: The format arguments for DESCRIPTION.


Example:
(with-estimated-time-progress (5 "This is just a example. Number of steps is ~a." 5)
  (dotimes (i 5) (perform-step)))

