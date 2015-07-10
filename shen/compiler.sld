;; Copyright (c) 2012-2015 Bruno Deferrari.  All rights reserved.
;; BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(define-library (shen compiler)
  (import (scheme base)
          (scheme read)
          (only (srfi 1) filter))

  (cond-expand
   (chibi
    (import (chibi match)
            (srfi 69)))
   (gauche
    (import (util match)
            (shen support gauche srfi-69))))

  (export kl->scheme
          register-function-arity
          function-arity
          nest-lambda)

  (include "impl/compiler.scm")
  (include "impl/declarations.scm"))
