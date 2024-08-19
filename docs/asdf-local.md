# asdf-local

A module for treating local systems compilation differently from third party systems.

[[source code]](../asdf-local.lisp)

- **Author**: Mariano Montone <marianomontone@gmail.com>
- **Version**: 0.1
- **Requires**: ASDF


 ASDF-LOCAL signals WARNINGs and STYLE-WARNINGs as errors for ASDF systems considered "local", and compiles third party systems normally.

 Usage:

 Set local systems or directories modifying *LOCAL-SYSTEMS*, *LOCAL-DIRECTORIES* and *NON-LOCAL-DIRECTORIES* variables.
 Then perform an ASDF:OPERATE.
 Local systems warnings are treated as errors, while non local system warnings are treated as warnings.
 OPERATE-LOCALLY is a utility function to perform an ASDF operation on an ASDF:SYSTEM, adding the system to the list of local systems in the process.

 Examples:

 (operate-locally 'asdf:compile-op :cl-forms :force t)
 (let ((*fail-on-style-warnings* nil)) (operate-locally 'asdf:load-op :cl-forms :force t))
 (asdf:operate 'asdf:load-op :ten :force t)
 (operate-locally 'asdf:load-op :ten :force t)
 (let ((*local-systems* (list :ten))) (asdf:operate 'asdf:load-op :ten :force t))
 (let ((*local-directories* (list (asdf:system-source-directory :ten)))) (asdf:operate 'asdf:load-op :ten :force t))

 To ignore style-warnings, bind *fail-on-style-warnings*:
 (let ((*fail-on-style-warnings* nil)) (operate-locally 'asdf:load-op :ten :force t))



