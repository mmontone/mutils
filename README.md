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
