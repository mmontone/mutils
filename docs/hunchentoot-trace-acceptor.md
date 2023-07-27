# hunchentoot-trace-acceptor

A Hunchentoot acceptor for tracing HTTP requests.

[[source code]](../hunchentoot-trace-acceptor.lisp)

- **Author**: Mariano Montone <marianomontone@gmail.com>
- **Version**: 0.1
- **Requires**: hunchentoot
- **Keywords**: debugging, web, hunchentoot


 A Hunchentoot acceptor for tracing HTTP requests.

 Example usage:

     (defclass my-acceptor (hunchentoot:easy-acceptor hunchentoot-trace:trace-acceptor)
       ())

 HTTP requests information is printed to `*standard-output*`.



## Variables
### \*trace-requests\*
Request tracing is enabled when this is T. Default is T.

### \*trace-session\*
Session tracing is enabled when this is T. Default is NIL.

## Classs
### trace-acceptor
A Hunchentoot acceptor for tracing requests

