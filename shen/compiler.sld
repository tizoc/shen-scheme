;; Copyright (c) 2012-2015 Bruno Deferrari.  All rights reserved.
;; BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(define-library (shen compiler)
  (import (scheme base)
          (scheme read)
          (srfi 69)
          (chibi match)
          (only (chibi) call-with-input-string)
          (only (srfi 1) filter))

  (export kl->scheme
          register-function
          register-function-arity
          function-arity
          nest-lambda
          *shen-functions*)

  (include "compiler.scm")
  (include "declarations.scm"))