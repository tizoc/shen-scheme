;; Copyright (c) 2012-2015 Bruno Deferrari.  All rights reserved.
;; BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(define-library (shen primitives)
  (import (scheme base) (scheme eval)
          (scheme write) (scheme file) (scheme time) (scheme char)
          (srfi 69)
          (only (meta) module-env %import)
          (only (chibi modules) load-module)
          (only (chibi pathname) path-resolve)

          (shen compiler))

  (cond-expand
    (chibi (import (chibi match)))
    (gauche (import (util match))))

  (export
   scm.register-function-arity
   scm.set-shen-environment!
   scm.l2r
   scm.call-nested
   scm.import-from-module
   scm.assert-boolean

   kl:intern
   kl:pos
   kl:tlstr
   kl:cn
   kl:str
   kl:string?
   kl:string->n
   kl:n->string
   kl:set
   kl:value
   kl:simple-error
   kl:error-to-string
   kl:cons
   kl:hd
   kl:tl
   kl:cons?
   kl:=
   kl:eval-kl
   kl:type
   kl:absvector
   kl:<-address
   kl:address->
   kl:absvector?
   kl:read-byte
   kl:write-byte
   kl:open
   kl:close
   kl:get-time
   kl:+
   kl:-
   kl:*
   kl:/
   kl:>
   kl:<
   kl:>=
   kl:<=
   kl:number?

   kl->scheme)

  (include "primitives.scm"))
