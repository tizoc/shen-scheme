chibi-scheme port of the Shen language
======================================

* `Shen <http://shenlanguage.org/>`_
* `chibi-scheme <http://code.google.com/p/chibi-scheme>`_

Shen is a portable functional programming language by `Mark Tarver <http://www.lambdassociates.org/>`_ that offers

- pattern matching,
- λ calculus consistency,
- macros,
- optional lazy evaluation,
- static type checking,
- an integrated fully functional Prolog,
- and an inbuilt compiler-compiler.

Running
-------

Version 0.6 of chibi-scheme is needed to run chibi-shen. Other versions may work, but testing and development are done against that version.

Copy all the .kl files to this directory and run::

    chibi-scheme -xshen -e'(shen.shen)'

This will launch a Shen REPL running on top of chibi-scheme.

License
-------

- http://shenlanguage.org/license.html
- Shen, Copyright © 2010-2012 Mark Tarver
- chibi-shen, Copyright © 2012 Bruno Deferrari
