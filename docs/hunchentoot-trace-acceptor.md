# hunchentoot-trace-acceptor

A Hunchentoot acceptor that traces requests.
[[source code]](../hunchentoot-trace-acceptor.lisp)

- **Requires**: hunchentoot
- **Version**: 0.1
- **Author**: Mariano Montone <marianomontone@gmail.com>


 A Hunchentoot acceptor that traces requests.

 Example usage:

 ```lisp
 (defclass my-acceptor (hunchentoot:easy-acceptor hunchentoot-trace:trace-acceptor)
   ())

 (hunchentoot:start (make-instance 'my-acceptor :port 5000))
 ```

 Result:

 HTTP requests information is printed to `*standard-output*`.



## Variables
### \*trace-requests\*
nil

### \*trace-session\*
nil

## Classs
### trace-acceptor
nil

