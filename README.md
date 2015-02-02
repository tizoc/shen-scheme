chibi-shen, a chibi-scheme port of the Shen language
====================================================

* [Shen](http://shenlanguage.org)
* [chibi-scheme](http://code.google.com/p/chibi-scheme>)

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

The resulting code will live under the `compiled/` directory.
  
Running
-------

Version 0.7 of chibi-scheme is needed to run chibi-shen. Other versions may work, but testing and development are done against that version.

To launch the Shen REPL do:

    chibi-scheme -h 500M -xshen -e'(shen.shen)'

License
-------

- Shen, Copyright © 2010-2015 Mark Tarver - [License](http://www.shenlanguage.org/license.pdf).
- chibi-shen, Copyright © 2012-2015 Bruno Deferrari under [BSD 3-Clause License](http://opensource.org/licenses/BSD-3-Clause).
