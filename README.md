Shen/Scheme, a Scheme port of the Shen language
====================================================

* [Shen](http://shenlanguage.org)
* [chez-scheme](https://cisco.github.io/ChezScheme)
* [chibi-scheme](http://synthcode.com/wiki/chibi-scheme)
* [gauche](http://practical-scheme.net/gauche/)
* [shen-scheme](https://github.com/tizoc/shen-scheme)

Shen is a portable functional programming language by [Mark Tarver](http://marktarver.com) that offers

- pattern matching,
- λ calculus consistency,
- macros,
- optional lazy evaluation,
- static type checking,
- an integrated fully functional Prolog,
- and an inbuilt compiler-compiler.

shen-scheme is a port of the Shen language that runs on top of Sheme implementations.

Right now the following implementations are supported:

* [chez-scheme](https://cisco.github.io/ChezScheme)

The following implementations were supported in version 0.15, but are not supported since version 0.16. Support may be added back in future releases.

* [chibi-scheme](http://synthcode.com/wiki/chibi-scheme)
* [gauche](http://practical-scheme.net/gauche/)

Building
--------

### Building from scratch

This step is only necessary if cloning from this repository, the release tarballs include pregenerated `.scm` files.

To build from source, obtain a [copy of the Shen kernel distribution](https://github.com/Shen-Language/shen-sources/releases) and copy the `.kl` files to the `kl/` directory of shen-scheme. Then with a working Shen implementation do:

    (load "scripts/build.shen")
    (build program "shen-chez.scm")

This will produce `.scm` files in the `compiled/` and a `shen-chez.scm` file in the current directory.

### Creating a runnable executable

The `shen-chez.scm` file can then be compiled into runnable file using Chez Scheme:

    (compile-script "shen-chez.scm" "shen-chez")

The resulting `shen-chez` file can be then copied to a directory in your PATH.

**TODO**: *add instructions for Windows*
  
Running
-------

`shen-chez` will start the Shen REPL. `shen-chez --script <some shen file>` will run a script.

Native Calls
------------

Scheme functions live under the `scm` namespace (`scm.` prefix). For example: `(scm.write [1 2 3 4])` invokes Scheme's `write` function with a list as an argument.

Because Scheme functions can have variable numbers of arguments and the code passed to `scm.` is not preprocessed, any imported function that is intended to support partial application has to be wrapped with a `defun`:

```
(0-) (defun my-for-each (F L) (scm.for-each F L))
my-for-each

(1-) (my-for-each (/. X (do (print (+ X X)) (nl))) [1 2 3 4 5])
2
4
6
8
10
0

(2-) (for-each (function print))
#<procedure>
```

Importing bindings from Scheme modules
--------------------------------------

[import expressions](https://cisco.github.io/ChezScheme/csug9.4/libraries.html#./libraries:h4) are supported through the `scm.` prefix. Names will be imported under the `scm.` namespace.

Example:

    (1-) (scm.import (rename (rnrs) (+ add-numbers)))
    #<void>

    (2-) (scm.add-numbers 1 2 3 4)
    10

License
-------

- Shen, Copyright © 2010-2015 Mark Tarver - [License](http://www.shenlanguage.org/license.pdf).
- shen-scheme, Copyright © 2012-2017 Bruno Deferrari under [BSD 3-Clause License](http://opensource.org/licenses/BSD-3-Clause).
