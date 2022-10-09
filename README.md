[![Shen Version](https://img.shields.io/badge/shen-33.1.1-blue.svg)](https://github.com/Shen-Language)
[![build](https://github.com/tizoc/shen-scheme/workflows/build/badge.svg)](https://github.com/tizoc/shen-scheme/actions?query=workflow%3Abuild)

Shen/Scheme, a Scheme port of the Shen language
====================================================

* [Shen](https://shen-language.github.io/)
* [chez-scheme](https://cisco.github.io/ChezScheme)
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

Binaries
--------

Starting with version 0.18, binaries are provided for Windows, Linux and OSX. See [releases](https://github.com/tizoc/shen-scheme/releases).

OSX users can also use homebrew to install Shen/Scheme:

```
$ brew install Shen-Language/homebrew-shen/shen-scheme
==> Installing shen-language/shen/shen-scheme
==> Downloading https://github.com/tizoc/shen-scheme/releases/download/0.17/shen-scheme-0.17-src.tar.gz
Already downloaded: /Users/bruno/Library/Caches/Homebrew/shen-scheme-0.17.tar.gz
==> Downloading https://github.com/cisco/ChezScheme/archive/v9.5.tar.gz
Already downloaded: /Users/bruno/Library/Caches/Homebrew/shen-scheme--chezscheme-9.5.tar.gz
==> make install prefix=/usr/local/Cellar/shen-scheme/0.17
  /usr/local/Cellar/shen-scheme/0.17: 7 files, 2.8MB, built in 1 minute 16 seconds
```

Building
--------

### Building from the source distribution

Running `make` should do the job. It will download and compile Chez under the `_build` directory, and then the `shen-scheme` binary and `shen.boot` boot files.

    make prefix=/opt/shen-scheme # optional prefix, defaults to /usr/local

then to install:

    make install

This will install the `shen-scheme` binary to `$(prefix)/bin/shen-scheme` and the boot file to `$(prefix)/lib/shen-scheme/shen.boot`.

To build on Windows, an environment with curl, 7zip, make and Visual Studio 2017 is needed (all installable with [chocolatey](https://chocolatey.org/)).

### Building from scratch

This step is only necessary if cloning from this repository, the release tarballs include pregenerated `.scm` files.

To build from source, obtain a [copy of the Shen kernel distribution](https://github.com/Shen-Language/shen-sources/releases) and copy the `.kl` files to the `kl/` directory of shen-scheme. Then with a working Shen implementation do:

    (load "scripts/build.shen")
    (build program "shen-scheme.scm")

This will produce `.scm` files in the `compiled/` directory and a `shen-scheme.scm` file in the current directory.

After doing this the procedure is the same as building from the source distribution.

Running
-------

`shen-scheme` will start the Shen REPL.
`shen-scheme script <some shen file>` will run a script.
`shen-scheme eval <shen expression>` will evaluate an expression.

Home and Boot file search path
------------------------------

Shen/Scheme will use as its *home directory* a path relative to the executable: `../lib/shen-scheme`.
For example, if the executable is `/usr/local/bin/shen-bin` then the *home directory* will be `/usr/local/lib/shen-scheme`.
This can be overriden by the `SHEN_SCHEME_HOME` environment variable.

By default, the boot file will be loaded from `<shen-scheme-home>/shen.boot`, but the location can be overriden with the `SHEN_SCHEME_BOOT` environment variable.

Native Calls
------------

Scheme functions live under the `scm` namespace (`scm.` prefix), and the names need to be wrapped with the `foreign` form in calls. For example: `((foreign scm.write) [1 2 3 4])` invokes Scheme's `write` function with a list as an argument.

Because Scheme functions can have variable numbers of arguments and the code passed to `scm.` is not preprocessed, any imported function that is intended to support partial application has to be wrapped with a `defun`:

```
(0-) (defun my-for-each (F L) ((foreign scm.for-each) F L))
my-for-each

(1-) (my-for-each (/. X (do (print (+ X X)) (nl))) [1 2 3 4 5])
2
4
6
8
10
0

(2-) (my-for-each (function print))
#<procedure>
```

Literal Scheme Code
-------------------

Scheme code can be compiled as-is with the `scm.` special form that takes a string with Scheme code as an argument.

Example:

```
(0-) ((foreign scm.) "(+ 1 2)")
3

(1-) ((foreign scm.) "(begin (display c#34;testc#34;) (newline))")
test
#<void>

(2-) ((foreign scm.) "(list #t #f (quote symbol) 'symbol)")
[true false symbol symbol]
```

Importing bindings from Scheme modules
--------------------------------------

[import expressions](https://cisco.github.io/ChezScheme/csug9.5/libraries.html#./libraries:h4) are supported through the `scm.` prefix. Names will be imported under the `scm.` namespace.

Example:

    (1-) ((foreign scm.import) (rename (rnrs) (+ add-numbers)))
    #<void>

    (2-) ((foreign scm.add-numbers) 1 2 3 4)
    10

License
-------

- Shen, Copyright © 2010-2022 Mark Tarver - [License](http://www.shenlanguage.org/license.pdf).
- shen-scheme, Copyright © 2012-2023 Bruno Deferrari under [BSD 3-Clause License](http://opensource.org/licenses/BSD-3-Clause).
