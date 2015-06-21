;; Copyright (c) 2012-2015 Bruno Deferrari.  All rights reserved.
;; BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(define-library (shen compiler)
  (import (scheme base)
          (scheme read)
          (srfi 69)
          (only (srfi 1) filter))

  (cond-expand
    (chibi (import (chibi match)))
    (gauche (import (util match))))

  (export kl->scheme
          register-function-arity
          function-arity
          nest-lambda)

  (include "compiler.scm")
  (include "declarations.scm"))