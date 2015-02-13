chibi-shen, a chibi-scheme port of the Shen language
====================================================

* [Shen](http://shenlanguage.org)
* [chibi-scheme](http://code.google.com/p/chibi-scheme>)
* [chibi-shen](https://github.com/tizoc/chibi-shen)

Shen is a portable functional programming language by [Mark Tarver](http://marktarver.com) that offers

- pattern matching,
- λ calculus consistency,
- macros,
- optional lazy evaluation,
- static type checking,
- an integrated fully functional Prolog,
- and an inbuilt compiler-compiler.

Building
--------

To precompile the `.kl` files into Scheme code run:

    make

The resulting code will live under the `shen/compiled/` directory.
  
Running
-------

Version 0.7 of chibi-scheme is needed to run chibi-shen. Other versions may work, but testing and development are done against that version.

To launch the Shen REPL do:

    chibi-scheme -Rshen.runner

To run a script do:

    chibi-scheme -Rshen.runner script.shen

The initial heap size can be increased by using chibi-scheme's `-h` option:

    chibi-scheme -h50M -Rshen.runner

The `(shen init)` module exports two functions, `shen.shen` for launching the shen REPL and `shen.load` for loading Shen scripts:

```
# cat test.shen
(print [1 2 3 4])
(nl)
# chibi-scheme
> (import (shen init))
> (shen.load "test.shen")
[1 2 3 4]
((1 2 3 4) 0)
> (shen.shen)

Shen, copyright (C) 2010-2015 Mark Tarver
www.shenlanguage.org, Shen 17
running under Scheme, implementation: chibi-scheme
port 0.10 ported by Bruno Deferrari


(0-) 
```

Native Calls
------------

Scheme functions live under the `scm` namespace (`scm.` prefix). For example: `(scm.write [1 2 3 4])` invokes Scheme's `write` function with a list as an argument.

To send literal, unprocessed code to the underlying interpreter the `scm.` form can be used:

```
(0-) (scm. "(scm.+ 1 2 3 4)")
10

(1-) (scm. "(scm.define (func-name x) (scm.display x) (scm.newline))")
#<undef>

(2-) (func-name "test")
test
#<undef>

```

Note that the `scm.` prefix is still required, because Scheme functions have been imported inside the Shen environment with an `scm.` prefix, and all compiled code runs inside this environment.

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

**Note on importing Scheme modules:**

As of chibi-shen 0.10 this does not work:

```
(scm. "(scm.import (prefix (srfi 27) scm.))")
```

Which means that for now external modules can't be imported in the REPL.

A temporary workaround is to edit `shen/init.sld` and add the necessary imports as described in [this comment](https://github.com/tizoc/chibi-shen/issues/1#issuecomment-72329117).

License
-------

- Shen, Copyright © 2010-2015 Mark Tarver - [License](http://www.shenlanguage.org/license.pdf).
- chibi-shen, Copyright © 2012-2015 Bruno Deferrari under [BSD 3-Clause License](http://opensource.org/licenses/BSD-3-Clause).
