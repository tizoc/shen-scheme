(define-library (shen overwrites-internal)
  (import (scheme base) (scheme file)
          (srfi 69)
          (only (chibi pathname) make-path)
          (only (chibi io) port->string)
          (only (shen primitives) kl:value full-path-for-file $$function-binding))

  (export
   $$read-file-as-bytelist
   $$read-file-as-string
   $$shen-sysfunc?
   $$shen-walk
   $$macroexpand)

  (include "overwrites-internal.scm"))