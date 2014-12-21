(define-library (shen compiler)
  (import (scheme base)
          (srfi 69)
          (chibi match)
          (only (srfi 1) filter))

  (export kl->scheme
          register-function
          register-function-arity
          function-arity
          nest-lambda
          *shen-functions*)

  (include "compiler.scm")
  (include "declarations.scm"))