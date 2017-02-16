;; Copyright (c) 2012-2015 Bruno Deferrari.  All rights reserved.
;; BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(define-library (shen compiler)
  (import (scheme base)
          (scheme read)
          (only (srfi 1) filter)
          (srfi 69))

  (cond-expand
   (chibi
    (import (chibi match)))
   (gauche
    (import (util match)
            (scheme cxr))))

  (export kl->scheme
          register-function-arity
          function-arity
          nest-lambda)

  (include "impl/compiler.scm")
  (include "impl/declarations.scm"))
