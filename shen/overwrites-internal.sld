(define-library (shen overwrites-internal)
  (import (scheme base) (scheme file)
          (srfi 69)
          (only (chibi pathname) make-path)
          (only (chibi io) port->string)
          (only (shen primitives) kl:value full-path-for-file scm.function-binding))

  (export
   scm.read-file-as-bytelist
   scm.read-file-as-string
   scm.shen-sysfunc?
   scm.shen-walk
   scm.macroexpand)

  (include "overwrites-internal.scm"))