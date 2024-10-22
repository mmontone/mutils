# mupaginator

A helper package for pagination of collections.

[[source code]](../mupaginator.lisp)

- **https**: //www.zacfukuda.com/blog/pagination-algorithm
- **Version**: 0.1
- **Requires**: cl-who, alexandria, trivial-types


 Helper package for implementing pagination of collections.
 
 Usage:

 Create a PAGINATION object using MAKE-PAGINATION function, passing the current page and a source for the pagination, either a SEQUENCE or a FUNCTION-DESIGNATOR that takes a page number and returns two values: the items of that page, and the total number of items.
 Then print that object to an HTML stream, using of the PRINT-PAGINATION functions, and passing HREF or ON-CLICK handlers for resolving urls/actions for the page buttons.

 ```lisp
 (defroute vanilla-pagination-test "/vanilla"
     ((page :parameter-type 'integer :init-form 1))
   (let ((pagination (mupaginator:make-pagination :current page :source (list-all-packages))))
     (with-html
       (:ul
        (dolist (item (mupaginator:pagination-current-items pagination))
          (who:htm (:li (who:str (package-name item))))))
       (mupaginator:print-pagination-bootstrap
        pagination
        :href (lambda (page-nr)
                (easy-routes:genurl 'vanilla-pagination-test :page page-nr))
        :stream html))))
 ```



## Functions
### make-pagination

```lisp
(&key ((:current #:current) 1) ((:page-size #:page-size) 10)
 ((:source #:source) nil))
```


### pagination-current

```lisp
(sb-kernel:instance)
```


### (setf pagination-current)

```lisp
(sb-kernel::value sb-kernel:instance)
```


### pagination-current-items

```lisp
(pagination)
```

Returns PAGINATION current page items and total pages.




### pagination-next

```lisp
(pagination)
```


### pagination-prev

```lisp
(pagination)
```


### pagination-total

```lisp
(pagination)
```


### print-pagination

```lisp
(pagination &optional (stream *standard-output*) &rest options)
```

Debug function for printing a text representation of PAGINATION.




### print-pagination-bootstrap

```lisp
(pagination &key href on-click (stream *standard-output*)
 (first-and-last-buttons t) (prev-and-next-buttons t) (use-ellipsis t)
 (padding 2))
```

Like PRINT-PAGINATION-HTML, but for Bootstrap framework.



See: [https://getbootstrap.com/docs/4.1/components/pagination/](https://getbootstrap.com/docs/4.1/components/pagination/)
### print-pagination-html

```lisp
(pagination &key href on-click (stream *standard-output*)
 (first-and-last-buttons t) (prev-and-next-buttons t) (use-ellipsis t)
 (padding 2))
```

Print PAGINATION to a vanilla HTML STREAM.

- **HREF**: a FUNCTION-DESIGNATOR that is called with a page number argument and should return the URL string for that page for the buttons.
- **ON-CLICK**: a FUNCTION-DESIGNATOR that is called with a page number argument and should return a Javascript string to use for the onclick event on the buttons.
- **STREAM**: the stream where to write the HTML to.
- **FIRST-AND-LAST-BUTTONS**: render first and last page buttons.
- **PREV-AND-NEXT-BUTTONS**: render previous and next page buttons.



### print-pagination-w3css

```lisp
(pagination &key href on-click (stream *standard-output*)
 (first-and-last-buttons t) (prev-and-next-buttons t) (use-ellipsis t)
 (padding 2))
```

Like PRINT-PAGINATION-HTML, but for W3CSS framework.



See: [https://www.w3schools.com/w3css/w3css_pagination.asp](https://www.w3schools.com/w3css/w3css_pagination.asp)
## Classes
### pagination
