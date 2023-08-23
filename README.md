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

Then a section with the module source code:

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

## Contributing

I welcome contributions of new modules. If you are interested in mutils shipping yours, create a pull request or attach your file with the module.

Modules should be general purpose and be compact enough to fit into a single file and package. Although they can also depend on other modules.

The module file should follow the format described in this document.

## Modules api (mutils package)

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

* [clhs-linker](docs/clhs-linker.md) - Replace Lisp terms in a file by hyperlinks to Common Lisp HyperSpec.
* [compiler-hooks](docs/compiler-hooks.md) - Provides hooks for Common Lisp compilation api.
* [compiler-info](docs/compiler-info.md) - Provides compiler info (specially from declarations) in a portable way.
* [debug-print](docs/debug-print.md) - A reader macro package for debug printing.
* [def-properties](docs/def-properties.md) - Portable extractor of information from Common Lisp definitions.
* [directory-module-loader](docs/directory-module-loader.md) - Loader of Lisp module files from directories.
* [estimated-time-progress](docs/estimated-time-progress.md) - Progress display with estimated time.
* [html2who](docs/html2who.md) - Parse HTML and create cl-who source code.
* [hunchentoot-errors](docs/hunchentoot-errors.md) - Augments Hunchentoot error pages and logs with request and session information.
* [hunchentoot-trace-acceptor](docs/hunchentoot-trace-acceptor.md) - A Hunchentoot acceptor for tracing HTTP requests.
* [if-star](docs/if-star.md) - The if* macro used in Allegro.
* [image-dimensions-reader](docs/image-dimensions-reader.md) - Get image dimensions (PNG/JPG) without loading the file.
* [lisp-critic-warnings](docs/lisp-critic-warnings.md) - Signal compiler warnings with lisp-critic critiques.
* [muprotocols](docs/muprotocols.md) - An implementation of protocols that plays nicely with Common Lisp type system.
* [mutils-docs](docs/mutils-docs.md) - Documentation generator for mutils.
* [mutils-utils](docs/mutils-utils.md) - General purpose utilities.
* [plump-xpath](docs/plump-xpath.md) - xpath extension for plump.
* [quicksearch](docs/quicksearch.md) - Search Engine Interface for Common Lisp.
* [type-annotations](docs/type-annotations.md) - Support for inline type annotations.
* [who-templates](docs/who-templates.md) - Templating system with CL-WHO. Supports inheritance.
