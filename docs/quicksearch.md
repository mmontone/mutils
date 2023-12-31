# quicksearch

Search Engine Interface for Common Lisp.

[[source code]](../quicksearch.lisp)

- **Author**: Takaya OCHIAI <#.(reverse "moc.liamg@lper.hcykt")>
- **Version**: 0.1
- **Requires**: anaphora, iterate, drakma, do-urlencode, yason, html-entities


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




## Functions
### ?

```lisp
(search-word &rest options)
```

? is abbreviation wrapper for function QUICKSEARCH.
search-word' must be a string, number or symbol. options' must be a
non-negative integer (as Cut-Off) and/or some keywords which consists of
some Option-Chars.



Options:
 * Cut-Off:
* The max number of printing results (default is 50).
* Option-Chars:
* d, D -- output Description
* u, U -- output URL
* q, Q -- search in Quicklisp
* c, C -- search in Cliki
* g, G -- search in GitHub
* b, B -- search in Bitbucket

Note:
 * Option-Char is idempotent (e.g. :dd <=> :d).
* (If  (:ref "OPTIONS' CONTAINS MORE THAN 2 CUT-OFFS, ONLY LAST ONE IS APPLYED.
 * THE ORDER OF OPTION-CHARS IS NOTHING TO DO WITH OUTPUT
   (E.G. :DU <=> :UD).
 * THE ORDER OF ")
   options' is nothing to do with output(except for some Cut-Offs).)
* If no search-space is specified, all spaces are specified(e.g. :d <=> :dqcgb).
* If at most one search-space is specified, then others are notspecified.

Examples:
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
### config

```lisp
(&key ((:maximum-columns-of-description max-cols) 80 max-cols-supplied?)
 ((:maximum-number-of-fetching-repositories max-repos) 50 max-repos-supplied?)
 (cache-size 4 cache-size-supplied?) (clear-cache? nil clear-cache-supplied?)
 (threading? t threading-supplied?)
 (quicklisp-verbose? nil quicklisp-verbose-supplied?))
```

Function CONFIG customizes printing, fetching or caching.
If CONFIG is called with no keyword, then it sets default values.



Keywords:

 * ( (:ref ":MAXIMUM-COLUMNS-OF-DESCRIPTION' (DEFAULT 80)
   THE VALUE MUST BE A PLUS INTEGER BIGGER THAN 5.
   IF THE LENGTH OF DESCRIPTION-STRING IS BIGGER THAN THIS VALUE,
   THEN OUTPUT OF DESCRIPTION IS INSERTED NEWLINE FOR EASY TO SEE.

 * ")
    :maximum-number-of-fetching-repositories' (default 50)This value controls the number of fetching repositories.The value must be a plus integer.Increasing this value, the number of fetching repositories increases,but also space & time does.)

 * ( (:ref ":CACHE-SIZE'
   THE VALUE MUST BE A PLUS INTEGER (DEFAULT 4).
   THIS VALUE IS THE NUMBER OF STORED PREVIOUS SEARCH-RESULTS.
   INCREASING THIS VALUE, THE NUMBER OF CACHING RESULTS INCREASES, BUT
   ALSO SPACE DOES.

 * ")
    :clear-cache?'The value must be a boolean (default NIL).If value is T, then clear all caches.)

 * ( (:ref ":THREADING?'
   THE VALUE MUST BE A BOOLEAN (DEFAULT T).
   IF VALUE IS NIL, THEN QUICKSEARCH BECOMES NOT TO USE THREADS FOR
   SEARCHING.

   NOTE:
     CURRENTLY ON SBCL (1.1.8), THREADS ARE PART OF THE DEFAULT BUILD ON
     X86[-64] LINUX ONLY. OTHER PLATFORMS (X86[-64] DARWIN (MAC OS X),
     X86[-64] FREEBSD, X86 SUNOS (SOLARIS), AND PPC LINUX)
     EXPERIMENTALLY SUPPORTS THREADS AND MUST BE EXPLICITLY ENABLED AT
     BUILD-TIME. FOR MORE DETAILS, PLEASE SEE [SBCL MANUAL](HTTP://WWW.SBCL.ORG/MANUAL/INDEX.HTML#THREADING).

 * ")
    :quicklisp-verbose?'The value must be a boolean (default NIL).If value is T, then outputs version of quicklisp and whether libraryhad installed your local.)

   Example:
     CL-REPL> (qs:config :QUICKLISP-VERBOSE? t)
     CL-REPL> (qs:? "json" :q)

     SEARCH-RESULTS: "json"

      Quicklisp: 2013-04-20   ;<- quicklisp version
       !cl-json               ;<- if library has been installed via quicklisp, print prefix "!".
       !cl-json.test
       com.gigamonkeys.json   ;<- if not, none.
       json-template
       st-json
     T

Note:
 * If you would prefer permanent configuration,for example, add codes something like the following in the CL initfile.

   In .sbclrc for SBCL, ccl-init.lisp for CCL:

   (ql:quickload :quicksearch)
   (qs:config :maximum-columns-of-description 50
              :maximum-number-of-fetching-repositories 20
              :cache-size 10
              :threading? nil
              :quicklisp-verbose? t)
### quicksearch

```lisp
(search-word &key (?web t) (?description nil) (?url nil) (?cut-off 50)
 (?quicklisp t) (?cliki t) (?github t) (?bitbucket t))
```

Search for CL projects with search-word' in Quicklisp, Cliki, GitHub
and BitBucket. search-word' must be a string, number or symbol (symbol
will be automatically converted into downcase-string).



Keywords:
 * (If  (:ref "?WEB' IS NIL, IT DOES NOT SEARCH IN CLIKI, GITHUB AND BITBUCKET.
 * IF ")
    ?quicklisp' is NIL, it does not search in Quicklisp (also
    (:ref "?CLIKI', ") ?github',  (:ref "?BITBUCKET').
 * AT LEAST ONE SEARCH-SPACE MUST BE SPECIFIED.
 * IF ")
    ?description' is T, it displays project's descriptions (exceptfor Quicklisp-search).)
* (If  (:ref "?URL' IS T, IT DISPLAY PROJECT'S URL.
 * ")
   ?cut-off' is the max number of printing repositories each space.)

Note:
 * (keyword  (:ref "?CUT-OFF' CONTROLS ONLY PRINTING RESULTS, NOTHING TO DO WITH
   THE MAXIMUM NUMBER OF FETCHING REPOSITORIES (SEE. FUNCTION CONFIG
   DOCUMENTATION).

 * ABOUT #\\SPACE IN ")
    search-word':In case `search-word' contains #\Space, Quicklisp-search isOR-search, whereas Cliki-search, GitHub-, BitBucket- is AND-search.e.g. (quicksearch "foo bar")Quicklisp-search for "foo" OR "bar",Cliki-search, GitHub-, BitBucket- for "foo" AND "bar".)

## Variables
### \*user-agent\*
This value tells the server who is requested (i.e. User-Agent header
value). If you are embedding Quicksearch in a larger application, you
should change the value of \*USER-AGENT\* to your application name and
URL.

