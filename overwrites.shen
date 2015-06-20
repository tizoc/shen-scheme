\\ Copyright (c) 2012-2015 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(define cd
  Val -> (let Dir (if (= Val "") "" (scm.string-append Val "/"))
              NewDir (scm.change-directory Dir)
           (set *home-directory* (scm.current-directory))))

(define hash
  Val Bound -> (let Res (scm.hash Val Bound)
                 (scm.if (scm.eq? 0 Res)
                     1
                     Res)))
(define not
  Val -> (scm.not Val))

(define boolean?
  Val -> (scm.boolean? Val))

(define integer?
  Val -> (scm.integer? Val))

(define variable?
  Val -> (scm.and
           (scm.symbol? Val)
           (scm.char-upper-case? (scm.string-ref (scm.symbol->string Val) 0))))

(define symbol?
  Val -> (scm.symbol? Val))

(define shen.pvar?
  V -> (scm.and
         (absvector? V)
         (scm.and
           (scm.> (scm.vector-length V) 0)
           (scm.eq? (scm.vector-ref V 0) shen.pvar))))

(define shen.numbyte?
  N -> (scm.and (scm.>= N 48) (scm.<= N 57)))

(define shen.byte->digit
  N -> (scm.- N 48))

(define shen.lookup-func
  F SymbolTable ->
    (let Entry (scm.assq F SymbolTable)
      (scm.if
       Entry
       (scm.cdr Entry)
       (simple-error (scm.string-append
                      (scm.symbol->string F)
                      " has no lambda expansion\n")))))

\\ Definitions for these are on shen/overwrites-internal.scm

(define shen.sysfunc?
  Val -> (scm.shen-sysfunc? Val))

(define read-file-as-bytelist
  Filename -> (scm.read-file-as-bytelist Filename))

(define read-file-as-string
  Filename -> (scm.read-file-as-string Filename))
