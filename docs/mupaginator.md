# mupaginator

A helper package for pagination of collections.

[[source code]](../mupaginator.lisp)

- **Version**: 0.1
- **Requires**: cl-who


 Helper package for implementing pagination of collections.

 Usage:



## Functions
### make-pagination

```lisp
(&key ((:current #:current) nil) ((:next #:next) nil) ((:prev #:prev) nil)
 ((:pages #:pages) nil) ((:total #:total) nil))
```


### print-pagination

```lisp
(pagination &optional (stream *standard-output*))
```


