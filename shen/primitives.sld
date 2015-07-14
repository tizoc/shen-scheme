;; Copyright (c) 2012-2015 Bruno Deferrari.  All rights reserved.
;; BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(define-library (shen primitives)
  (import (scheme base)
          (scheme eval)
          (scheme write)
          (scheme file)
          (scheme time)
          (scheme char)
          (shen compiler))

  (cond-expand
   (chibi
    (import (chibi match)
            (srfi 69)
            (only (chibi pathname) path-resolve)
            (only (meta) module-env %import)
            (only (chibi modules) load-module))
    (include "impl/chibi/import.scm")
    (export import-from-module))
   (gauche
    (import (util match)
            (gauche base)
            (scheme cxr)
            (shen support gauche srfi-69)
            (only (file util)
                  build-path expand-path absolute-path?))
    (include "impl/gauche/import.scm")
    (export import-from-module)
    (begin
      (define (path-resolve subpath base)
        (let ((subpath (expand-path subpath)))
          (if (absolute-path? subpath)
              subpath
              (build-path (expand-path base) subpath)))))))

  (export

   register-function-arity
   set-shen-environment!
   l2r
   assert-boolean
   full-path-for-file

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

  (include "impl/primitives.scm"))
