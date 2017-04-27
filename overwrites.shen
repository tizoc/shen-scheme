\\ Copyright (c) 2012-2015 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(define cd
  Val -> (let Dir (if (= Val "")
                      (scm.current-directory)
                      (scm.string-append Val "/"))
              NewDir (scm.change-directory Dir)
           (set *home-directory* (scm.current-directory))))

(define hash
  Val Bound -> (scm.hash Val Bound))

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

(define dict
  Size -> (scm.make-hash-table))

(define dict?
  X -> (scm.hash-table? X))

(define dict-count
  Dict -> (scm.hash-table-size Dict))

(define dict->
  Dict K V -> (scm.hash-table-set! Dict K V))

(define <-dict/or
  Dict K Or -> (scm.hash-table-ref Dict K Or))

(define dict-rm
  Dict K -> (scm.hash-table-delete! Dict K))

(define dict-fold
  Dict F Init -> (scm.hash-table-fold Dict F Init))

(define dict-keys
  Dict -> (scm.hash-table-keys Dict))

(define dict-values
  Dict -> (scm.hash-table-values Dict))

(define exit
  Code -> (scm.exit Code))

(define value/or
  Var Or -> (scm.value/or Var Or))

(define <-address/or
  Vector N Or -> (if (>= N (scm.vector-length Vector))
                     (thaw Or)
                     (scm.vector-ref Vector N)))

\\ Definitions for these are on shen/overwrites-internal.scm

(define read-file-as-bytelist
  Filename -> (scm.read-file-as-bytelist Filename))

(define read-file-as-charlist
  Filename -> (scm.read-file-as-bytelist Filename))

(define read-file-as-string
  Filename -> (scm.read-file-as-string Filename))
