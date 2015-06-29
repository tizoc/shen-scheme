;; Copyright (c) 2012-2015 Bruno Deferrari.  All rights reserved.
;; BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(define-library (shen primitives)
  (import (scheme base)
          (scheme eval)
          (scheme write)
          (scheme file)
          (scheme time)
          (scheme char)
          (srfi 69)
          (shen compiler))

  (cond-expand
    (chibi
      (import (chibi match)
              (only (chibi pathname) path-resolve)
              (only (meta) module-env %import)
              (only (chibi modules) load-module))
      (include "chibi/import.scm")
      (export scm.import-from-module))
    (gauche
      (import (util match)
              (rename (file util)
                (resolve-path path-resolve)))))

  (export
   scm.register-function-arity
   scm.set-shen-environment!
   scm.l2r
   scm.assert-boolean

   kl:intern
   kl:str
   kl:set
   kl:value
   kl:error-to-string
   kl:=
   kl:eval-kl
   kl:open
   kl:close
   kl:get-time

   kl->scheme)

  (include "primitives.scm"))
