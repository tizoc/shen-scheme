;; Copyright (c) 2012-2015 Bruno Deferrari.  All rights reserved.
;; BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(define-library (shen overwrites-internal)
  (import (scheme base) (scheme file)
          (srfi 69)
          (chibi string)
          (only (chibi pathname) make-path)
          (only (chibi io) port->string)
          (only (shen primitives) kl:value full-path-for-file scm.function-binding))

  (export
   scm.read-file-as-bytelist
   scm.read-file-as-string
   scm.shen-sysfunc?
   scm.shen-grammar_symbol?)

  (include "overwrites-internal.scm"))