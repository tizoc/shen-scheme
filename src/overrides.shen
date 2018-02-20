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

(define pr
  String Sink -> (trap-error
                  (let _ (if (scm.textual-port? Sink)
                             (scm.put-string Sink String)
                             (scm.put-bytevector Sink (scm.string->utf8 String)))
                       _ (scm.and (scm.should-flush? Sink)
                                  (scm.flush-output-port Sink))
                    String)
                  (/. E String)))

(define variable?
  Val -> (scm.and
          (scm.symbol? Val)
          (scm.char-upper-case? (scm.string-ref (scm.symbol->string Val) 0))))

(define shen.analyse-symbol?
  S -> (scm.analyse-symbol? S))

(define symbol?
  Val -> (and (scm.symbol? Val)
              (shen.analyse-symbol? (str Val))))

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
  Size -> (scm.make-equal-hashtable Size))

(define shen.dict?
  X -> (scm.hashtable? X))

(define shen.dict-count
  Dict -> (scm.hashtable-size Dict))

(define shen.dict->
  Dict K V -> (do (scm.hashtable-set! Dict K V)
                  V))

(define shen.<-dict
  Dict K -> (let Res (scm.hashtable-ref Dict K $%value-not-found%$)
              (if (scm.eq? Res $%value-not-found%$)
                  (error "value ~A not found in dict~%" K)
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

\* read-file-as-* defined in prelude.scm *\

(define read-file-as-bytelist
  Filename -> (scm.read-file-as-bytelist Filename))

(define shen.read-file-as-charlist
  Filename -> (scm.read-file-as-bytelist Filename))

(define read-file-as-string
  Filename -> (scm.read-file-as-string Filename))

\* To print location of errors *\

(package shen [scm.error-location]

(define toplevel-display-exception
  E -> (let Msg (error-to-string E)
            Loc (scm.error-location E)
        (if (interactive-error? E Loc Msg)
            (output "~A" Msg)
            (output "Exception in ~A: ~A" Loc Msg))))

(define interactive-error?
  E shen.toplineread_loop "line read aborted" -> true
  E shen.f_error "aborted" -> true
  E shen.typecheck-and-evaluate "type error" -> true
  _ _ _ -> false)

)
