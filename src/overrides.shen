\* Copyright (c) 2012-2018 Bruno Deferrari.  All rights reserved. *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(define cd
  "" -> (cd (value shen.*initial-home-directory*))
  Dir -> (let NewDir (scm.current-directory Dir)
           (set *home-directory* (scm.current-directory))))

(define hash
  Val Bound -> (scm.fxmod (scm.equal-hash Val) Bound))

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
         (scm.fx>? (scm.vector-length V) 0)
         (= (scm.vector-ref V 0) shen.pvar))))

(define shen.numbyte?
  N -> (scm.and (scm.fx>=? N 48) (scm.fx<=? N 57)))

(define shen.byte->digit
  N -> (scm.fx- N 48))

(define shen.dict
  Size -> (scm.make-eqv-hashtable Size))

(define shen.dict?
  X -> (scm.hashtable? X))

(define shen.dict-count
  Dict -> (scm.hashtable-size Dict))

(define shen.dict->
  Dict K V -> (do (scm.hashtable-set! Dict K V)
                  V))

(define shen.<-dict/or
  Dict K Or -> (let Res (scm.hashtable-ref Dict K $%value-not-found%$)
                 (if (scm.eq? Res $%value-not-found%$)
                     (thaw Or)
                     Res)))

(define shen.dict-rm
  Dict K -> (do (scm.hashtable-delete! Dict K)
                K))

\* hashtable-fold defined in prelude.scm *\
(define shen.dict-fold
  Dict F Init -> (scm.hashtable-fold Dict F Init))

(define shen.dict-keys
  Dict -> (scm.hashtable-keys Dict))

(define shen.dict-values
  Dict -> (scm.hashtable-values Dict))

(define shen.value/or
  Var Or -> (scm.value/or Var Or))

(define shen.<-address/or
  Vector N Or -> (if (>= N (scm.vector-length Vector))
                     (thaw Or)
                     (scm.vector-ref Vector N)))

\* read-file-as-* defined in prelude.scm *\

(define read-file-as-bytelist
  Filename -> (scm.read-file-as-bytelist Filename))

(define shen.read-file-as-charlist
  Filename -> (scm.read-file-as-bytelist Filename))

(define read-file-as-string
  Filename -> (scm.read-file-as-string Filename))

\* To print location of errors *\

(package shen [scm.error-location]

(define loop
  -> (do (initialise_environment)
         (prompt)
         (trap-error
          (read-evaluate-print)
          (/. E (output "Exception in ~A: ~A" (scm.error-location E) (error-to-string E))))
         (if (value *continue-repl-loop*)
             (loop)
             exit)))

)