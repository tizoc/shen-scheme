shen-scheme, a Scheme port of the Shen language
====================================================

* [Shen](http://shenlanguage.org)
* [chibi-scheme](http://synthcode.com/wiki/chibi-scheme)
* [gauche](http://practical-scheme.net/gauche/)
* [shen-scheme](https://github.com/tizoc/chibi-shen)

Shen is a portable functional programming language by [Mark Tarver](http://marktarver.com) that offers

- pattern matching,
- λ calculus consistency,
- macros,
- optional lazy evaluation,
- static type checking,
- an integrated fully functional Prolog,
- and an inbuilt compiler-compiler.

shen-scheme is a port of the Shen language that runs on top of Sheme implementations. Right now the following implementations are supported:

* [chibi-scheme](http://synthcode.com/wiki/chibi-scheme)
* [gauche](http://practical-scheme.net/gauche/)

Building
--------

To precompile the `.kl` files into Scheme code run:

    make

The resulting code will live under the `shen/compiled/` directory.
  
Running
-------

### chibi-scheme

Version 0.7.3 or newer of chibi-scheme is needed to run shen-scheme. Other versions may work, but testing and development are done against that version.

To launch the Shen REPL do:

    chibi-scheme -Rshen.runner

To run a script do:

    chibi-scheme -Rshen.runner script.shen

or

    chibi-scheme -Rshen.runner script.shen arg1 arg2 arg3 ...

to pass arguments to it.

The initial heap size can be increased by using chibi-scheme's `-h` option:

    chibi-scheme -h50M -Rshen.runner

For convenience a `bin/shen-chibi` script is included for POSIX systems:

    ./bin/shen-chibi script.shen arg1 arg2 arg3 ...

### Gauche

Version 0.95 or newer of Gauche is required to run shen-scheme.

To launch the Shen REPL do:

    gosh -I. -mshen.runner shen/runner.sld

To run a script do:

    gosh -I. -mshen.runner shen/runner.sld script.shen

or

    gosh -I. -mshen.runner shen/runner.sld script.shen arg1 arg2 arg3 ...

to pass arguments to it.

For convenience a `bin/shen-gauche` script is included for POSIX systems:

    ./bin/shen-gauche script.shen arg1 arg2 arg3 ...

### (shen init) module

The `(shen init)` module exports the following functions:

* `kl:shen.shen` for launching the shen REPL.
* `kl:shen.quiet-load` for loading Shen scripts.
* `kl:eval-kl` for evaluating Klambda code.
* `kl:eval` for evaluating Shen code.

```
# cat test.shen
(print [1 2 3 4])
(nl)
# chibi-scheme
> (import (shen init))
> (kl:shen.quiet-load "test.shen")
[1 2 3 4]
((1 2 3 4) 0)
> (kl:shen.shen)

Shen, copyright (C) 2010-2015 Mark Tarver
www.shenlanguage.org, Shen 19.2
running under Scheme, implementation: chibi-scheme
port 0.14 ported by Bruno Deferrari


(0-) 
```

Native Calls
------------

Scheme functions live under the `scm` namespace (`scm.` prefix). For example: `(scm.write [1 2 3 4])` invokes Scheme's `write` function with a list as an argument.

To send literal, unprocessed code to the underlying interpreter the `scm.` form can be used:

```
(0-) (scm. "(+ 1 2 3 4)")
10

(1-) (scm. "(define (func-name x) (display x) (newline))")
#<undef>

(2-) (scm.func-name "test")
test
#<undef>

```

Because Scheme functions can have variable numbers of arguments and the code passed to `scm.` is not preprocessed, any imported function that is intended to support partial application has to be wrapped with a `defun`:

```
(3-) (defun for-each (F L) (scm.for-each F L))
for-each

(4-) (for-each (/. X (do (print (+ X X)) (nl))) [1 2 3 4 5])
2
4
6
8
10
#<undef>

(5-) (for-each (function print))
#<procedure #f>
```

Importing bindings from Scheme modules
--------------------------------------

The `scm.import-from-module` function imports bindings from Scheme modules into Shen's environment.

It takes two arguments: a module identifier, and a list of lists of which the first element is a symbol with the name the imported binding is going to have inside Shen's environment, and the second the name the binding inside the module. If the exported name is the same as the original name, a symbol can be used in place of the list.

Example (chibi):

```
(6-) (scm.import-from-module [srfi 27] [[random-integer random-integer] [random-real random-real]])
[[random-integer random-integer] [random-real random-real]]

(7-) (scm.random-integer 1000)
927

(8-) (scm.random-real)
0.155211571676289
```

Example (gauche):

```
(0-) (scm.import-from-module file.util [home-directory])
[home-directory]

(1-) (scm.home-directory)
"/Users/bruno"
```

Extensions to core Shen
-----------------------

##### Command line arguments

* `(command-line)` with type `--> (list string)`: Returns a list containing all comand line elements. The first element is the program name, and the remaining elements are the arguments passed to it.

##### Process exit

* `(exit ExitCode)` with type `number --> unit`: Exits the process using the specified exit code.

##### Standard error stream

* `(sterror)` with type `--> (stream out)`: Returns the standard error stream.

##### File operations

* `(open-append Filepath)` with type `string --> (stream out)`: Opens a file for output in "append" mode that doesn't truncate the file and returns a stream positioned at the end of the file.

##### Stream position operations

* `(stream-position Stream)` with type `(stream A) --> number`: Returns the stream position.
* `(stream-set-position Stream AbsolutePosition)` with type `(stream A) --> number --> number`: Sets the stream position.
* `(stream-set-position-from-current Stream RelativePosition)` with type `(stream A) --> number --> number`: Sets the stream position to a value relative to the current position.
* `(stream-set-position-from-end Stream RelativePosition)` with type `(stream A) --> number --> number`: Sets the stream position to a value relative to the end position.

License
-------

- Shen, Copyright © 2010-2015 Mark Tarver - [License](http://www.shenlanguage.org/license.pdf).
- chibi-shen, Copyright © 2012-2015 Bruno Deferrari under [BSD 3-Clause License](http://opensource.org/licenses/BSD-3-Clause).
