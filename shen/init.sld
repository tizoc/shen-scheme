;; Copyright (c) 2012-2015 Bruno Deferrari.  All rights reserved.
;; BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(define-library (shen init)
  (import

   (rename (shen primitives)
     (kl:intern           intern)
     (kl:str              str)
     (kl:set              set)
     (kl:value            value)
     (kl:error-to-string  error-to-string)
     (kl:=                =)
     (kl:eval-kl          eval-kl)
     (kl:open             open)
     (kl:close            close)
     (kl:get-time         get-time))

   (shen overwrites-internal)

   (prefix (scheme base) scm.)
   (prefix (scheme char) scm.)
   (prefix (scheme file) scm.)
   (prefix (scheme eval) scm.)
   (prefix (scheme process-context) scm.)
   (prefix (only (srfi 69) hash) scm.)
   (prefix (only (chibi) current-environment) scm.)
   (prefix (only (chibi filesystem)
                 change-directory current-directory
                 open open/append open/write open/create
                 open-output-file-descriptor)
           scm.)
   (prefix (only (chibi pathname) path-resolve) scm.)
   (prefix (only (chibi io)
                 seek/end seek/cur seek/set
                 set-file-position! file-position)
           scm.))

  (export shen.shen
          shen.quiet-load)

  (include "init.scm")

  ;; Avoid warning about shen.demod not being defined yet
  (begin (scm.define (shen.demod Val) Val))

  (include "compiled/toplevel.kl.scm")
  (include "compiled/core.kl.scm")
  (include "compiled/sys.kl.scm")
  (include "compiled/sequent.kl.scm")
  (include "compiled/yacc.kl.scm")
  (include "compiled/reader.kl.scm")
  (include "compiled/prolog.kl.scm")
  (include "compiled/track.kl.scm")
  (include "compiled/load.kl.scm")
  (include "compiled/writer.kl.scm")
  (include "compiled/macros.kl.scm")
  (include "compiled/declarations.kl.scm")
  (include "compiled/types.kl.scm")
  (include "compiled/t-star.kl.scm")
  (include "compiled/extras.kl.scm")

  (begin
    (cd "")))
