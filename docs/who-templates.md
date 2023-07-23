# who-templates

Templating system with CL-WHO. Supports inheritance.

[[source code]](../who-templates.lisp)

- **Requires**: cl-who
- **Version**: 0.1
- **Author**: Mariano Montone <marianomontone@gmail.com>


 Templating system with CL-WHO. Supports inheritance.

 Example:

 Base template example:

 (deftemplate base-1 ()
   (&args title)
   (:html
    (:head
     (:title (who:str (or title "WHO TEMPLATES")))
     (block styles
       (:link :rel "stylesheet" :href "/bootstrap.css")))
    (:body
     (block body)
     (block scripts))))

 (render-template-to-string 'base-1)
 (render-template-to-string 'base-1 :title "lala")

 Inheritance/block overwrite. Calls to parent:

 (deftemplate foo (:parent base-1)
   (block body
     (:h1 (who:str "Foo"))))

 (render-template-to-string 'foo)

 (deftemplate bar (:parent base-1)
   (block body
     (:h1 (who:str "Bar")))
   (block styles
     (parent)
     (:link :rel "stylesheet" :href "/bar.css")))

 (render-template-to-string 'bar)

 (deftemplate baz (:parent bar)
   (block scripts
     (parent)
     (:script :type "text/javacript"
              (who:str "...javascript code..."))))

 (render-template-to-string 'baz)

 Args:

 (deftemplate hello (:parent base-1)
   (block body
     (:h1 (who:str (targ :hello)))))

 (render-template-to-string 'hello :hello "Hello!!")

 (deftemplate hello-2 (:parent base-1)
   (block body
     (&args hello)
     (:h1 (who:str hello))
     (:h2 (who:str hello))))

 (render-template-to-string 'hello-2 :hello "Hi!!")

 (deftemplate hello-3 (:parent base-1)
   (block body
     (with-targs (hello)
       (:h1 (who:str hello))
       (:h2 (who:str hello)))))

 (render-template-to-string 'hello-3 :hello "Hi!!")

 Include:

 (deftemplate snippet ()
   (:p (who:str "This stuff has been included")))

 (deftemplate include (:parent base-1)
   (block body
     (include 'snippet)))

 (render-template-to-string 'include)



## Functions
### block

```lisp
(sb-c::name &rest sb-c::forms)
```

BLOCK name form*



Evaluate the FORMS as a PROGN. Within the lexical scope of the body,
RETURN-FROM can be used to exit the form.
### include

```lisp
(template-name)
```


### parent

```lisp
(&optional (block *block*) (template (template-parent *template*)))
```

Render the parent block




### render-template

```lisp
(name stream &rest args)
```


### render-template-to-string

```lisp
(name &rest args)
```


### targ

```lisp
(symbol)
```


## Macros
### deftemplate

```lisp
(name args &body body)
```



### with-targs

```lisp
(args &body body)
```



