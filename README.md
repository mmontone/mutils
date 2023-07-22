# MUTILS

A collection of Common Lisp utilities provided as modules.

The modules provided are small enough to fit in a single file and Common Lisp package, and an ASDF system definition may not be justified for them.

## Usage

In your ASDF system, `depend-on` `:mutils`, and then use `:require` to pick the modules to load.

Example:

```lisp
(asdf:defsystem :mutils-example
  :depends-on (:mutils 
                 (:require :lisp-critic-warnings)
                 (:require :auto-gensym)))
```

## Modules

Modules are single Lisp files that follow a format similar to Emacs packages.

They start with a commented section:

* The first line is header with the module name and a short description.
* Then a Copyright and license, followed by some module properties, like author, version, requirements, and more.
* A commentary section with a long description of the module, with usage instructions and examples.

Then the code starts:

* `require` calls are placed at the top.
* The definition of a package and the source code of the module.
* A `provide` call at the end of the file.

The template looks like this:

```
;;; <module name> --- <module short description>

;; Copyright (C) 2023 <author>. All rights reserved.

;; This work is licensed under the terms of the <license> license.  
;; For a copy, see <https://opensource.org/licenses/<license>>.

;; Author: <author> <email>
;; Version: <module version>
;; Requires: <required modules separated by comma>

;;; Commentary:

;; <long module description with usage instructions and examples>

;;; Code:

(require :<requirementA>)
(require :<requirementB>)

(defpackage :<module-package>
  (:use :cl))

(in-package :<module-package>)

... <module source code> ...

(provide :<module-name>)
```

An example module:

```lisp
;;; plump-xpath --- xpath extension for plump.

;; Copyright (C) 2023 Mariano Montone. All rights reserved.

;; This work is licensed under the terms of the MIT license.  
;; For a copy, see <https://opensource.org/licenses/MIT>.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Version: 0.1
;; Requires: plump, xpath

;;; Commentary:

;; xpath extension for plump.

;;; Code:

(require :plump)
(require :xpath)

(defpackage :plump-xpath
  (:use :cl))

(in-package :plump-xpath)

...

(provide :plump-xpath)
```

## mutils utilities

### describe-module

```lisp
(module-name)
```

Print a description of module.

### describe-modules

```lisp
()
```

Print a description of available mutils modules.

### generate-readme

```lisp
()
```

Generate a README file with information about available modules.

### list-modules

```lisp
(&optional (return :name))
```

List mutils modules.
RETURN can be:
- :name . Just returns the name of the modules, as a keyword. Default.
- :details. Parses the modules and returns its details.

### parse-lisp-module-file

```lisp
(file)
```

Parse a Lisp module file.

Returns the name of the module, its short description,
its properties (author, requirements, keywords, etc.),
its long description/comment with instructions of usage, etc.


## List of modules

* [who-templates](#who-templates) - Templating system with CL-WHO. Supports inheritance.
* [quicksearch](#quicksearch) - Search Engine Interface for Common Lisp.
* [plump-xpath](#plump-xpath) - xpath extension for plump.
* [lisp-critic-warnings](#lisp-critic-warnings) - Signal compiler warnings with lisp-critic critiques.
* [html2who](#html2who) - Parse HTML and create cl-who source code.
* [estimated-time-progress](#estimated-time-progress) - Progress display with estimated time.
* [directory-module-loader](#directory-module-loader) - Loader of Lisp module files from directories.
* [def-properties](#def-properties) - Portable extractor of information from Common Lisp definitions.
* [compiler-info](#compiler-info) - Provides compiler info (specially from declarations) in a portable way.
* [compiler-hooks](#compiler-hooks) - Provides hooks for Common Lisp compilation api.
* [auto-gensym](#auto-gensym) - Clojure style AUTO-GENSYM macro.


## Details of modules
### who-templates 


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



### quicksearch 


 Quicksearch is a search-engine-interface for Common Lisp.
 The goal of Quicksearch is to find the CL library quickly.
 For example, if you will find the library about json, just type `(qs:? 'json)` at REPL.

 The function `quicksearch` searches for CL projects in Quicklisp, Cliki,
 GitHub and BitBucket, then outputs results in REPL.
 The function `?` is abbreviation wrapper for `quicksearch`.

 Examples
 --------

 ##### Null result:

     CL-REPL> (qs:? "supercalifragilisticexpialidocious") ;<=> (qs:quicksearch "supercalifragilisticexpialidocious")
     NIL

  * If it raises a threading error, probably your CL system might be not support threads.
    In this case, please type `(qs:config :threading? nil)` at REPL, then try it again.
  * If search-results is null, then just return NIL.


 ##### Simple search:

     CL-REPL> (qs:? "crypt") ;<=> (qs:quicksearch "crypt")

     SEARCH-RESULTS: "crypt"

      Quicklisp
       crypt

      Cliki
       ARC4
       JARW
       VLM_on_Linux

      GitHub
       cl-crypt
       cl-crypto
       cl-crypto
       cryptography
       cryptoschool
       Cryptopsaras
     T

  * If search-results is not null, then results are printed and return T.
  * Since bitbucket-result is null, it is not printed.


 ##### Description:

     CL-REPL> (qs:? 'Crypt :d) ;<=> (qs:quicksearch 'Crypt :?description t)

     SEARCH-RESULTS: "crypt"
     =======================

      Quicklisp
      ---------
       crypt
           http://quickdocs.org/cl-crypt/

      Cliki
      -----
       ARC4
           A Common Lisp implementation of ARC4, a Cryptography code, can be found on
           the
       JARW
           Dr John AR Williams' utilities
       VLM_on_Linux
           Instructions for running the Symbolics VLM virtual machine on Linux

      GitHub
      ------
       cl-crypt
           Common-Lisp implementation of unix crypt function
       cl-crypto
           A common lisp package of ciphers, public-key algorithms, etc.
       cl-crypto
           Pure lisp crypto, written from specs
       cryptography
           implementations of ciphers for cryptography class
       cryptoschool
           Lisp files related to the cryptography class being taught by Ben Warner
       Cryptopsaras
           Reads files generated by Acinonyx.
     T

  * A symbol (as search-word) is automatically converted into a downcase-string.
  * If option `:d` is on, then the description of the project is printed (QuickDocs-url for Quicklisp-search).
  * The function QUICKSEARCH's options are redundant, but explanatory.
  * The function ?'s options are not explanatory, but minimum.


 ##### URL, Space, Cutoff:

     CL-REPL> (qs:? "crypt" :ug 4) ;<=> (qs:quicksearch "crypt"
                                   ;                    :?url t :?cut-off 4
                                   ;                    :?quicklisp nil :?cliki nil :?bitbucket nil)

     SEARCH-RESULTS: "crypt"
     =======================

      GitHub
      ------
       cl-crypt
           https://github.com/renard/cl-crypt
       cl-crypto
           https://github.com/bgs100/cl-crypto
       cl-crypto
           https://github.com/billstclair/cl-crypto
       cryptography
           https://github.com/MorganBauer/cryptography
       .......> 2
     T

  * If option `:u` is on, then the project's url is printed.
  * If option `:g` is on, then only github-results are printed
    (also `:q` - quicklisp, `:c` - cliki, `:b` - bitbutcket. these options are addable).
  * If cut-off is supplied (above 4), then the number of output results is bellow cut-off
    (the number (above 2) after `.......>` is number of remains).
  * The order of options is nothing to do with search-result
    (e.g. `:ug 4` <=> `4 :gu` <=> `:u 4 :g` <=> ...).


 ##### Config:

     CL-REPL> (qs:? 'lisp-koans :du 1) ;<=> (qs:quicksearch 'lisp-koans
                                       ;                    :?description t :?url t :?cut-off 1)

     SEARCH-RESULTS: "lisp-koans"
     ============================

      GitHub
      ------
       lisp-koans
           https://github.com/google/lisp-koans
           Common Lisp Koans is a language learning exercise in the same vein as the
           ruby koans, python koans and others.   It is a port of the prior koans
           with some modifications to highlight lisp-specific features.  Structured
           as ordered groups of broken unit tests, the project guides the learner
           progressively through many Common Lisp language features.
       .......> 4
     T

     CL-REPL> (qs:config :maximum-columns-of-description 50)
     Current maximum columns of description: 50
     T

     CL-REPL> (qs:? 'lisp-koans :du 1)

     SEARCH-RESULTS: "lisp-koans"
     ============================

      GitHub
      ------
       lisp-koans
           https://github.com/google/lisp-koans
           Common Lisp Koans is a language learning
           exercise in the same vein as the ruby koans,
           python koans and others.   It is a port of
           the prior koans with some modifications to
           highlight lisp-specific features.
           Structured as ordered groups of broken unit
           tests, the project guides the learner
           progressively through many Common Lisp
           language features.
       .......> 4
     T

  * `:maximum-columns-of-description` controls in printing description (default is 80).


 Reference Manual
 ----------------

 #### [function] QUICKSEARCH _search-word_ _&key_ _?web_ _?description_ _?url_ _?cut-off_ _?quicklisp_ _?cliki_ _?github_ _?bitbucket_

 QUICKSEARCH searches for CL projects with _search-word_ in Quicklisp, Cliki, GitHub and BitBucket.
 _search-word_ must be a string, number or symbol (symbol will be automatically converted into downcase-string).


 ##### Keywords:

  * If _?web_ is NIL, it does not search in Cliki, GitHub and BitBucket.
  * If _?quicklisp_ is NIL, it does not search in Quicklisp (also _?cliki_, _?github_, _?bitbucket_).
  * At least one search-space must be specified.
  * If _?description_ is T, it displays project's descriptions (QuickDocs-url for Quicklisp-search).
  * If _?url_ is T, it display project's url.
  * _?cut-off_ is the max number of printing repositories each space.


 ##### Note:

  * _?cut-off_ controls only printing results,
    nothing to do with the max number of fetching repositories.
    see. function CONFIG documentation

  * About #\Space in _search-word_:

    In case _search-word_ contains #\Space, Quicklisp-search is OR-search,
    whereas Cliki-search, GitHub-, BitBucket- is AND-search.

    e.g. (quicksearch "foo bar")
      Quicklisp-search for "foo" OR "bar".
      Cliki-search, GitHub-, BitBucket- for "foo" AND "bar".


 #### [function] ? _search-word_ _&rest_ _options_

 ? is abbreviation wrapper for function QUICKSEARCH.
 _search-word_ must be a string, number or symbol.
 _options_ must be a non-negative integer (as Cut-Off) and/or some keywords which consists of some Option-Chars.


 ##### Examples:

     (? "crypt")
     <=>
     (quicksearch "crypt" :?description nil :?url nil :?cut-off 50
                          :?quicklisp t :?cliki t :?github t :?bitbucket t)

     (? "crypt" :du 10)
     <=>
     (quicksearch "crypt" :?description T :?url T :?cut-off 10
                          :?quicklisp t :?cliki t :?github t :?bitbucket t)

     (? "crypt" 20 :g :d)
     <=>
     (quicksearch "crypt" :?description T :?url nil :?cut-off 20
                          :?quicklisp nil :?cliki nil :?github T :?bitbucket nil)


 ##### Options:

  * Cut-Off:
    * The max number of printing results (default is 50).

  * Option-Chars:
    * d, D -- output Description (or QuickDocs-url for Quicklisp-search)
    * u, U -- output URL
    * q, Q -- search in Quicklisp
    * c, C -- search in Cliki
    * g, G -- search in GitHub
    * b, B -- search in Bitbucket


 ##### Note:

  * Option-Char is idempotent (e.g. :dd <=> :d).
  * The order of Option-Chars is nothing to do with output. (e.g. :du <=> :ud)
  * If _options_ contains more than 2 Cut-Offs, only last one is applied.
  * The order of _options_ is nothing to do with output (except for some Cut-Offs).
  * If no search space is specified, all spaces is specified (e.g. :d <=> :dqcgb)
  * If at most one search space is specified, then others are not specified.


 #### [function] CONFIG _&key_ _maximum-columns-of-description_ _maximum-number-of-fetching-repositories_ _cache-size_ _clear-cache?_ _threading?_ _quicklisp-verbose?_

 Function CONFIG customizes printing, fetching or caching.


 ##### Keywords:

  * _:maximum-columns-of-description_
    The value must be a plus integer bigger than 5.
    If the length of description-string is bigger than this value,
    then output of description is inserted newline for easy to see.
    Default value is 80.

  * _:maximum-number-of-fetching-repositories_
    This value controls the number of fetching repositories.
    The value must be a plus integer.
    Increasing this value, the number of fetching repositories increases, but also space & time does.
    Default value is 50.

  * _:cache-size_
    The value must be a plus integer.
    This value is the number of stored previous search-results.
    Increasing this value, the number of caching results increases, but also space does.
    Default value is 4.

  * _:clear-cache?_
    The value must be a boolean.
    If value is T, then clear all caches.

  * _:threading?_
    The value must be a boolean (default T).
    If value is NIL, then QUICKSEARCH becomes not to use threads for searching.

    Note:
      Currently in SBCL (1.1.8), threads are part of the default build on x86[-64] Linux only.
      Other platforms (x86[-64] Darwin (Mac OS X), x86[-64] FreeBSD, x86 SunOS (Solaris),
      and PPC Linux) experimentally supports threads and must be explicitly enabled at build-time.
      For more details, please see [SBCL manual](http://www.sbcl.org/manual/index.html#Threading).

  * _quicklisp-verbose?_
    The value must be a boolean (default NIL).
    If value is T, then outputs version of quicklisp and whether library had installed your local.

    Example:
 ```
      CL-REPL> (qs:config :quicklisp-verbose? T)
      CL-REPL> (qs:? "json" :q)

      SEARCH-RESULTS: "json"

       Quicklisp: 2013-04-20   ;<- quicklisp version
        !cl-json               ;<- if library has installed via quicklisp, print prefix "!".
        !cl-json.test
        com.gigamonkeys.json   ;<- if not, none.
        json-template
        st-json
      T
 ```


 ##### Note:

 If you would prefer permanent config, for example,
 add codes something like the following in the CL init file.

 In `.sbclrc` for SBCL, `ccl-init.lisp` for CCL:

     (ql:quickload :quicksearch)
     (qs:config :maximum-columns-of-description 50
                :maximum-number-of-fetching-repositories 20
                :cache-size 2
                :threading? nil
                :quicklisp-verbose? t)


 #### [special variable] \*USER-AGENT\*

 \*user-agent\* tells the server who is requested (i.e. User-Agent header value).
 If you are embedding quicksearch in a larger application, you should
 change the value of \*user-agent\* to your application name and URL.


 TODO
 ----

 - ADD search-space: Common-Lisp.net
 - ADD search-space: Google code




### plump-xpath 


 xpath extension for plump.



### lisp-critic-warnings 


 Signal compiler warnings with lisp-critic critiques.



### html2who 


 Parse HTML and create cl-who source code.

 Usage:

(html5-parser:parse-html5 #p"/vagrant/admin/index.html" :dom :who :strictp nil)
(html5-parser:parse-html5-fragment #p"/vagrant/admin/index.html" :dom :who :strictp nil)
(html5-parser:parse-html5 #p"/vagrant/admin/index.html" :dom :xmls :strictp nil)


### estimated-time-progress 


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



### directory-module-loader 


 Loader of Lisp module files from directories.
 Lisp module files are similar to Emacs packages.
 requires at the top, and provide at the bottom.
 and package information in source code comments.



### def-properties 


 Portable extractor of information from Common Lisp definitions.



### compiler-info 


 Provides compiler info (specially from declarations) in a portable way.



### compiler-hooks 


 Provides hooks for Common Lisp compilation api.



### auto-gensym 


 Clojure style AUTO-GENSYM macro.



