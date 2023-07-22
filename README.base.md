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

Modules are single Lisp files that follow a format similar to [Emacs Packages](https://www.gnu.org/software/emacs/manual/html_node/elisp/Simple-Packages.html).

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
