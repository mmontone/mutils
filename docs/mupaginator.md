# mupaginator

A helper package for pagination of collections.

[[source code]](../mupaginator.lisp)

- **https**: //www.zacfukuda.com/blog/pagination-algorithm
- **Version**: 0.1
- **Requires**: cl-who, alexandria


 Helper package for implementing pagination of collections.

 Usage:



## Functions
### page-start-end

```lisp
(page page-size total)
```

Utility function for calculating start and end for PAGE, PAGE-SIZE and TOTAL.



Example usage:
    (apply #'subseq my-seq (multiple-value-list page page-size (length my-seq)))
### paginate

```lisp
(current total &key (use-ellipsis t) (padding 2) (include-first-and-last t))
```

Create a PAGINATION structure that can be then printed to HTML.
Args:
- CURRENT is the current page number, between 1 and TOTAL.
- TOTAL is the total number of pages.
- USE-ELLIPSIS: an :ELLIPSIS keyword is included when appropiate when enabled.
- PADDING: the list of buttons has length (PADDING * 2) + 1.
- INCLUDE-FIRST-AND-LAST: buttons for first and last page are included.




### print-pagination

```lisp
(pagination &optional (stream *standard-output*))
```

Debug function for printing a text representation of PAGINATION.




### print-pagination-bootstrap

```lisp
(pagination &key href on-click (stream *standard-output*)
 (first-and-last-buttons t) (prev-and-next-buttons t))
```

Like PRINT-PAGINATION-HTML, but for Bootstrap framework.




### print-pagination-html

```lisp
(pagination &key href on-click (stream *standard-output*)
 (first-and-last-buttons t) (prev-and-next-buttons t))
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
 (first-and-last-buttons t) (prev-and-next-buttons t))
```

Like PRINT-PAGINATION-HTML, but for W3CSS framework.




