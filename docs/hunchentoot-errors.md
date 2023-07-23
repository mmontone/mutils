# hunchentoot-errors

Augments Hunchentoot error pages and logs with request and session information.

[[source code]](../hunchentoot-errors.lisp)

- **Requires**: hunchentoot
- **Version**: 0.1
- **Author**: Mariano Montone <marianomontone@gmail.com>


 Augments Hunchentoot error pages and logs with request and session information.
 ### Usage

 Subclass your acceptor from `HUNCHENTOOT-ERRORS:ERRORS-ACCEPTOR`.

 When `hunchentoot:*show-lisp-errors-p*` is on, you get HTTP request and session information printed in errors pages and logs, like:

 ```
 Backtrace for: #<SB-THREAD:THREAD "hunchentoot-worker-127.0.0.1:46428" RUNNING {1002007DE3}>
 0: (TRIVIAL-BACKTRACE:PRINT-BACKTRACE-TO-STREAM #<SB-IMPL::CHARACTER-STRING-OSTREAM {1003C82953}>)
 1: ((FLET "FORM-FUN-4" :IN HUNCHENTOOT::GET-BACKTRACE))
 2: (HUNCHENTOOT::GET-BACKTRACE)
 3: ((FLET "H0" :IN HUNCHENTOOT:HANDLE-REQUEST) #<SIMPLE-ERROR "sdf" {1003C827F3}>)
 4: (SB-KERNEL::%SIGNAL #<SIMPLE-ERROR "sdf" {1003C827F3}>)
 5: (ERROR "sdf")
 6: (INVOICE-ENGINE::ADMIN/USERS/CREATE)
 7: ((LAMBDA NIL :IN EASY-ROUTES::PROCESS-ROUTE))
 8: (EASY-ROUTES::CALL-WITH-DECORATORS NIL #<CLOSURE (LAMBDA NIL :IN EASY-ROUTES::PROCESS-ROUTE) {1003C7C57B}>)
 9: ((LAMBDA NIL :IN EASY-ROUTES::CALL-WITH-DECORATORS))
 ...
 31: (SB-THREAD::CALL-WITH-MUTEX #<CLOSURE (FLET SB-THREAD::WITH-MUTEX-THUNK :IN SB-THREAD::NEW-LISP-THREAD-TRAMPOLINE) {7FD95C27ED9B}> #<SB-THREAD:MUTEX "thread result lock" owner: #<SB-THREAD:THREAD "hunchentoot-worker-127.0.0.1:46428" RUNNING {1002007DE3}>> NIL T NIL)
 32: (SB-THREAD::NEW-LISP-THREAD-TRAMPOLINE #<SB-THREAD:THREAD "hunchentoot-worker-127.0.0.1:46428" RUNNING {1002007DE3}> NIL #<CLOSURE (LAMBDA NIL :IN BORDEAUX-THREADS::BINDING-DEFAULT-SPECIALS) {1001FF7FBB}> NIL)
 33: ("foreign function: call_into_lisp")
 34: ("foreign function: new_thread_trampoline")

 HTTP REQUEST:
   uri: /admin/users/new
   method: POST
   post parameters:
     name: asdf
     username: asdf
     email: sdf@asdfasdf.com
     password: asdfasdf

 SESSION:
   FLASH-MESSAGES: NIL
   ROLE: "superadmin"
   USER: 3
   FORWARD-URL: "/"
 ```



## Classs
### errors-acceptor
Subclasses of this acceptor augment Hunchentoot error pages and logs with request and session information.

