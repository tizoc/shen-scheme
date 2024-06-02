\* Copyright (c) 2012-2021 Bruno Deferrari.  All rights reserved. *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(define hash
  Val Bound -> ((foreign scm.fxmod) ((foreign scm.equal-hash) Val) Bound))

(define not
  Val -> ((foreign scm.not) Val))

(define boolean?
  Val -> ((foreign scm.boolean?) Val))

(define integer?
  Val -> ((foreign scm.integer?) Val))

(define pr
  String Sink -> (trap-error
                  (let P (if ((foreign scm.textual-port?) Sink)
                             ((foreign scm.or) (value *hush*) ((foreign scm.put-string) Sink String))
                             ((foreign scm.or) (value *hush*) ((foreign scm.put-bytevector) Sink ((foreign scm.string->utf8) String))))
                       F ((foreign scm.and)
                            (not (value *hush*))
                            ((foreign scm.should-flush?) Sink)
                            ((foreign scm.flush-output-port) Sink))
                    String)
                  (/. E String)))

(define variable?
  Val -> ((foreign scm.and)
          ((foreign scm.symbol?) Val)
          ((foreign scm.char-upper-case?) ((foreign scm.string-ref) ((foreign scm.symbol->string) Val) 0))))

(define shen.analyse-symbol?
  S -> ((foreign scm.analyse-symbol?) S))

(define symbol?
  Val -> (and ((foreign scm.symbol?) Val)
              (shen.analyse-symbol? (str Val))))

(define shen.pvar?
  V -> ((foreign scm.and)
        (absvector? V)
        ((foreign scm.and)
         ((foreign scm.fx>?) ((foreign scm.vector-length) V) 0)
         (= ((foreign scm.vector-ref) V 0) shen.pvar))))

(define shen.numbyte?
  N -> ((foreign scm.and) ((foreign scm.fx>=?) N 48) ((foreign scm.fx<=?) N 57)))

(define shen.byte->digit
  N -> ((foreign scm.fx-) N 48))

(define shen.dict
  Size -> ((foreign scm.make-equal-hashtable) Size))

(define shen.dict?
  X -> ((foreign scm.hashtable?) X))

(define shen.dict-count
  Dict -> ((foreign scm.hashtable-size) Dict))

(define shen.dict->
  Dict K V -> (do ((foreign scm.hashtable-set!) Dict K V)
                  V))

(define shen.<-dict
  Dict K -> (let Res ((foreign scm.hashtable-ref) Dict K $%value-not-found%$)
              (if ((foreign scm.eq?) Res $%value-not-found%$)
                  (error "value ~A not found in dict~%" K)
                  Res)))

(define shen.dict-rm
  Dict K -> (do ((foreign scm.hashtable-delete!) Dict K)
                K))

\* hashtable-fold defined in prelude.scm *\
(define shen.dict-fold
  Dict F Init -> ((foreign scm.hashtable-fold) Dict F Init))

(define shen.dict-keys
  Dict -> ((foreign scm.hashtable-keys) Dict))

(define shen.dict-values
  Dict -> ((foreign scm.hashtable-values) Dict))

\* read-file-as-* defined in prelude.scm *\

(define read-file-as-bytelist
  Filename -> ((foreign scm.read-file-as-bytelist) ((foreign scm.string-append) (value *home-directory*) Filename)))

(define read-file-as-string
  Filename -> ((foreign scm.read-file-as-string) ((foreign scm.string-append) (value *home-directory*) Filename)))

\* tuples *\

(define @p
  X Y -> ((foreign scm.vector) shen.tuple X Y))

\* vectors *\

(define vector
  N -> (let Vector ((foreign scm.make-vector) (+ N 1) (fail))
            ZeroStamp (address-> Vector 0 N)
          Vector))

(define shen.char-stinput? S -> false)
(define shen.char-stoutput? S -> false)

\* factorise *\

\\ Branches are pushed into a stack instead of being evaluated
\\ Then when the parent defun is converted into Scheme code, these
\\ defuns are compiled and embedded into the parent definition.

(define shen.push-factorised-branch
  KL -> (let BS (trap-error (value shen.*branches-stack*) (/. E []))
          (set shen.*branches-stack* [KL | BS])))

(define shen.eval-factorised-branch
  [defun BranchName Args Body] -> (shen.push-factorised-branch [defun BranchName Args Body]))

\* To print location of errors *\

(package shen [scm.error-location]

(define toplevel-display-exception
  E -> (let Msg (error-to-string E)
            Loc ((foreign scm.error-location) E)
        (if (interactive-error? E Loc Msg)
            (output "~A" Msg)
            (output "Exception in ~A: ~A" Loc Msg))))

(define show-exceptions?
  -> (trap-error (value *show-exceptions*) (/. E false)))

(define interactive-error?
  _ _ _ -> true where (not (show-exceptions?))
  E abort _ -> true
  E shen.string-match _ -> true
  E shen.toplineread_loop "line read aborted" -> true
  E shen.f_error "aborted" -> true
  E shen.typecheck-and-evaluate "type error" -> true
  E shen.unwind-types _ -> true
  _ _ _ -> false)

)
