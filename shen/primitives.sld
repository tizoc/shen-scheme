;; Copyright (c) 2012-2015 Bruno Deferrari.  All rights reserved.
;; BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(define-library (shen primitives)
  (import (scheme base) (scheme eval)
          (scheme write) (scheme file) (scheme time) (scheme char)
          (srfi 69)
          (chibi string)
          (only (chibi) call-with-output-string)
          (only (chibi pathname) make-path)
          (only (chibi filesystem) file-exists?)
          (only (chibi io) port->string read-u8)

          (shen compiler))

  (export
   scm.set-shen-environment!
   scm.function-binding
   scm.l2r
   scm.call-nested
   scm.function

   kl:if
   kl:and
   kl:or
   kl:cond
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
   kl:trap-error
   kl:error-to-string
   kl:cons
   kl:hd
   kl:tl
   kl:cons?
   kl:defun
   kl:lambda
   kl:let
   kl:=
   kl:eval-kl
   kl:freeze
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
