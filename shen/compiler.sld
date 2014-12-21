(define-library (shen compiler)
  (import (scheme base) (scheme eval)
          (scheme write) (scheme file) (scheme time) (scheme char)
          (srfi 69)
          (chibi match) (chibi string)
          (only (chibi) call-with-output-string)
          (only (srfi 1) filter)
          (only (chibi pathname) make-path)
          (only (chibi filesystem) file-exists?)
          (only (chibi io) port->string read-u8))

  (export kl->scheme
          register-function
          register-function-arity
          function-arity
          nest-lambda
          *shen-functions*)

  (include "compiler.scm")
  (include "declarations.scm"))