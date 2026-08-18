\* Copyright (c) 2012-2021 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(package shen-scheme
 [release debug wpo unsafe
  tc+ tc-
  define defun defprolog declare defmacro datatype synonyms package
  native-unit native-compiletime-group
  runtime compiletime source-kl quote eval update-lambda-table
  lambda fn
  _scm.prefix-op scm.dynamic-wind scm.hashtable-copy
  scm.shen-scheme-native-call-with-new-compiletime-environment
  scm.shen-scheme-native-call-with-compiletime-environment
  scm.read-file-as-bytelist]

(set *native-compiler-state-depth* 0)

(define native-copy-property-vector
  D -> ((foreign scm.hashtable-copy) D true))

(define with-native-compiler-state
  F -> (F) where (> (value *native-compiler-state-depth*) 0)
  F -> ((foreign scm.shen-scheme-native-call-with-new-compiletime-environment)
        (freeze (with-native-compiler-state* F))))

(define with-native-compiler-state*
  F -> (let Old (value *property-vector*)
            New (native-copy-property-vector Old)
            Macros (value *macros*)
            SigF (value shen.*sigf*)
            Synonyms (value shen.*synonyms*)
            Demod (value shen.*demodulation-function*)
            Datatypes (value shen.*datatypes*)
            AllDatatypes (value shen.*alldatatypes*)
            UserDefs (value shen.*userdefs*)
            PatternHandlers
            (value shen.x.programmable-pattern-matching.*pattern-handlers*)
            PatternHandlerNames
            (value shen.x.programmable-pattern-matching.*pattern-handlers-reg*)
         ((foreign scm.dynamic-wind)
          (freeze
           (do (set *property-vector* New)
               (set *native-compiler-state-depth* 1)))
          F
          (freeze
           (do (set *property-vector* Old)
               (set *macros* Macros)
               (set shen.*sigf* SigF)
               (set shen.*synonyms* Synonyms)
               (set shen.*demodulation-function* Demod)
               (set shen.*datatypes* Datatypes)
               (set shen.*alldatatypes* AllDatatypes)
               (set shen.*userdefs* UserDefs)
               (set shen.x.programmable-pattern-matching.*pattern-handlers*
                    PatternHandlers)
               (set shen.x.programmable-pattern-matching.*pattern-handlers-reg*
                    PatternHandlerNames)
               (set *native-compiler-state-depth* 0))))))

(define with-native-compiletime-environment
  F -> ((foreign scm.shen-scheme-native-call-with-compiletime-environment) F))

(define native-register-pattern-handler
  F _ -> F where (element?
                   F
                   (value shen.x.programmable-pattern-matching.*pattern-handlers-reg*))
  F Handler
  -> (do (set shen.x.programmable-pattern-matching.*pattern-handlers-reg*
              [F | (value
                    shen.x.programmable-pattern-matching.*pattern-handlers-reg*)])
         (set shen.x.programmable-pattern-matching.*pattern-handlers*
              [Handler | (value
                          shen.x.programmable-pattern-matching.*pattern-handlers*)])
         F))

(define native-unregister-pattern-handler
  F -> F where (not (element?
                     F
                     (value
                      shen.x.programmable-pattern-matching.*pattern-handlers-reg*)))
  F -> (shen.x.programmable-pattern-matching.unregister-handler F))

(define native-scheme-path
  O -> (@s O ".scm"))

(define native-compile-profile
  release -> release
  debug -> debug
  wpo -> wpo
  unsafe -> unsafe
  "release" -> release
  "debug" -> debug
  "wpo" -> wpo
  "unsafe" -> unsafe
  P -> (error "native compiler expected profile release, debug, wpo, or unsafe, got: ~S~%" P))

(define native-compile-profile-options
  P -> (native-compile-profile-options* (native-compile-profile P)))

(define native-compile-profile-options*
  release -> [2 0 false false false]
  debug -> [2 2 true true false]
  wpo -> [2 0 false false true]
  unsafe -> [3 0 false false false])

(define native-compile-options
  -> (native-compile-profile-options release))

(define native-effective-wpo?
  _ true -> true
  [_ _ _ _ WPO] _ -> WPO)

(define read-file-unprocessed
  F -> (read-bytelist-unprocessed (read-file-as-bytelist F)))

(define native-read-file-unprocessed
  F -> (read-bytelist-unprocessed
        ((foreign scm.read-file-as-bytelist) F)))

(define read-bytelist-unprocessed
  Bs -> (trap-error (compile (/. X (shen.<s-exprs> X)) Bs)
                    (/. X (shen.reader-error (value shen.*residue*)))))

(define native-shen->kl
  [define F | _] -> (error "~A is not a legitimate function name~%" F)
    where (shen.sysfunc? F)
  [define F | Rs] -> (shen.shendef->kldef F Rs))

(define native-init->kl
  F -> (shen.shen->kl-h F))

(define native-define-type-table
  [define F { | X] -> [(shen.typetable [define F { | X])]
  _ -> [])

(define native-type-table
  Fs -> (mapcan (function native-define-type-table) Fs))

(define native-type-declaration-form
  [F T] -> [(_scm.prefix-op declare) [quote F] [quote T]])

(define native-type-declaration-forms
  Ts -> (map (function native-type-declaration-form) Ts))

(define native-eval-form
  F -> [(_scm.prefix-op eval) [quote F]])

(define native-raw-compiletime-form
  [define | _] -> []
  [defprolog | _] -> []
  [declare F T] -> [(native-type-declaration-form [F T])]
  [declare | X] -> (error "native compiler expected top-level declare with a name and type, got: ~S~%"
                          [declare | X])
  [defmacro N | Rs] -> [(native-eval-form [defmacro N | Rs])]
  [defmacro | X] -> (error "native compiler expected top-level defmacro with a name and rules, got: ~S~%"
                           [defmacro | X])
  [datatype N | Rs] -> [(native-eval-form [datatype N | Rs])]
  [datatype | X] -> (error "native compiler expected top-level datatype with a name and rules, got: ~S~%"
                           [datatype | X])
  [synonyms | X] -> [(native-eval-form [synonyms | X])]
  _ -> [])

(define native-raw-compiletime-forms
  Fs -> (mapcan (function native-raw-compiletime-form) Fs))

(define native-first-compiletime-forms
  [] -> []
  [F | Fs] -> (let CT (native-raw-compiletime-form F)
                (if (= [] CT)
                    (native-first-compiletime-forms Fs)
                    CT)))

(define native-compiletime-form
  R Xs -> (let CT (native-raw-compiletime-form R)
            (if (= [] CT)
                (native-first-compiletime-forms Xs)
                CT)))

(define native-package-forms
  [] -> []
  [[native-package N Xs Fs] | Ps]
  -> [[(_scm.prefix-op shen.record-external) [quote N] [quote Xs]]
      [(_scm.prefix-op shen.record-internal) [quote N] [quote Xs] [quote Fs]]
      | (native-package-forms Ps)])

(define native-expand-forms
  Fs -> (native-expand-forms* Fs [] [] []))

(define native-macroexpanded-form
  [native-macroexpanded F _] -> F)

(define native-macroexpanded-steps
  [native-macroexpanded _ Fs] -> Fs)

(define native-macroexpand
  F -> (let Ms (map (/. X (tl X)) (value *macros*))
         (native-macroexpand* F Ms Ms [])))

(define native-macroexpand*
  F [] _ Xs -> [native-macroexpanded F (reverse Xs)]
  F [M | Ms] All Xs
  -> (let X (shen.walk M F)
       (if (= F X)
           (native-macroexpand* F Ms All Xs)
           (native-macroexpand* X All All [X | Xs]))))

(define native-expand-forms*
  [] Xs CTs Ps -> [native-expanded (reverse Xs) (reverse CTs)
                  (native-package-forms (reverse Ps))]
  [[package null _ | Bs] | Fs] Xs CTs Ps
  -> (native-expand-forms* (append Bs Fs) Xs CTs Ps)
  [[package N E | Bs] | Fs] Xs CTs Ps
  -> (let Es (eval E)
          Qs (shen.package-symbols (str N) Es Bs)
       (do (shen.record-external N Es)
           (shen.record-internal N Es Bs)
           (native-expand-forms* (append Qs Fs) Xs CTs [[native-package N Es Bs] | Ps])))
  [[package | X] | _] _ _ _
  -> (error "native compiler expected top-level package with a name and externals, got: ~S~%"
            [package | X])
  [F | Fs] Xs CTs Ps
  -> (let MX (native-macroexpand F)
          M (native-macroexpanded-form MX)
          Ms (native-macroexpanded-steps MX)
       (if (shen.packaged? M)
           (native-expand-forms* [M | Fs] Xs CTs Ps)
           (native-expand-forms* Fs [M | Xs]
                                 [(native-compiletime-form F (append Ms [M])) | CTs] Ps))))

(define native-process-expanded
  [native-expanded Xs CTs Ps]
  -> (do (shen.find-arities Xs)
         (let Ts (shen.find-types Xs)
              Fs (map (/. X (shen.process-applications X Ts)) Xs)
           [native-source-data Fs (mapcan (/. X X) CTs) Ps
            (native-type-table Fs)])))

(define native-module-source-mode
  [module-source M _] -> M)

(define native-module-source-path
  [module-source _ P] -> P)

(define native-check-source-form
  F T -> (let Check (shen.typecheck F T)
           (if (= Check false)
               (shen.type-error)
               skip)))

(define native-source-state-effect
  [declare F T] -> (declare F T)
  _ -> skip)

(define native-check-source-forms
  [] -> []
  [F Colon T | Fs]
  -> (do (native-check-source-form F T)
         (native-source-state-effect F)
         [F | (native-check-source-forms Fs)])
    where (= Colon (intern ":"))
  [F | Fs]
  -> (do (native-check-source-form F (protect A))
         (native-source-state-effect F)
         [F | (native-check-source-forms Fs)]))

(define native-source-state-effects
  [] -> skip
  [F Colon _ | Fs]
  -> (do (native-source-state-effect F)
         (native-source-state-effects Fs))
    where (= Colon (intern ":"))
  [F | Fs]
  -> (do (native-source-state-effect F)
         (native-source-state-effects Fs)))

(define native-assumption-table->types
  [] -> []
  [F T | Ts] -> [[F T] | (native-assumption-table->types Ts)])

(define native-check-module-source
  tc+ Fs Table -> (do (shen.assumetypes Table)
                      (native-check-source-forms Fs))
  tc- Fs _ -> (do (native-source-state-effects Fs)
                  Fs))

(define native-process-module-source
  [module-source M P]
  -> (native-process-module-source*
      M (native-expand-forms (native-read-file-unprocessed P))))

(define native-process-module-source*
  M [native-expanded Xs CTs Ps]
  -> (do (shen.find-arities Xs)
         (let Ts (shen.find-types Xs)
              Fs (map (/. X (shen.process-applications X Ts)) Xs)
              Table (if (= M tc+)
                        (mapcan (/. F (shen.typetable F)) Fs)
                        [])
           (let RuntimeFs (native-check-module-source M Fs Table)
             [native-source-data
              RuntimeFs
              (append
              (native-type-declaration-forms
               (native-assumption-table->types Table))
               (mapcan (/. X X) CTs))
              Ps
              []]))))

(define native-read-sources
  Ss -> (native-process-expanded
         (native-expand-forms (native-read-source-forms Ss))))

(define native-read-source-forms
  [] -> []
  [S | Ss] -> (append (read-file-unprocessed S)
                      (native-read-source-forms Ss)))

(define native-processed-defines
  [] -> []
  [[define F | Rs] | Fs] -> [[define F | Rs] | (native-processed-defines Fs)]
  [_ | Fs] -> (native-processed-defines Fs))

(define native-last-defines
  Ds -> (reverse (native-last-defines* (reverse Ds) [])))

(define native-last-defines*
  [] _ -> []
  [[define F | _] | Ds] Seen -> (native-last-defines* Ds Seen)
    where (element? F Seen)
  [[define F | Rs] | Ds] Seen
  -> [[define F | Rs] | (native-last-defines* Ds [F | Seen])])

(define native-processed-inits
  [] -> []
  [[define | _] | Fs] -> (native-processed-inits Fs)
  [[declare | _] | Fs] -> (native-processed-inits Fs)
  [[F | Rs] | Fs] -> [[F | Rs] | (native-processed-inits Fs)]
  [_ | Fs] -> (native-processed-inits Fs))

(define native-record-defun
  [defun F | X] -> (put F shen-scheme.native-defun [defun F | X]))

(define native-record-defuns
  [] -> skip
  [D | KL] -> (do (native-record-defun D)
                  (native-record-defuns KL)))

(define native-defun-lambda
  [defun _ As B] -> (native-args-lambda As B))

(define native-args-lambda
  [] _ -> (error "native pattern handler must take at least one argument~%")
  [A] B -> [lambda A B]
  [A | As] B -> [lambda A (native-args-lambda As B)])

(define native-pattern-handler-lambda
  F -> (trap-error
        (native-defun-lambda (get F shen-scheme.native-defun))
        (/. X [fn F])))

(define native-pattern-effect-kl
  [shen.x.programmable-pattern-matching.register-handler F]
  -> [shen-scheme.native-register-pattern-handler
      F
      (native-pattern-handler-lambda F)]
  [shen.x.programmable-pattern-matching.unregister-handler F]
  -> [shen-scheme.native-unregister-pattern-handler F]
  _ -> [])

(define native-init-form->kl
  F -> (let E (native-pattern-effect-kl F)
         (if (= E []) (native-init->kl F) E)))

(define native-processed-init-kl
  Fs -> (map (function native-init-form->kl) (native-processed-inits Fs)))

(define native-pattern-effect-kl-forms
  [] -> []
  [F | Fs] -> (let E (native-pattern-effect-kl F)
                (if (= E [])
                    (native-pattern-effect-kl-forms Fs)
                    [E | (native-pattern-effect-kl-forms Fs)])))

(define native-stage-pattern-effect
  [shen-scheme.native-unregister-pattern-handler F]
  -> (native-unregister-pattern-handler F)
  E -> (eval-kl E))

(define native-stage-pattern-effects
  [] -> skip
  [E | Es] -> (do (native-stage-pattern-effect E)
                  (native-stage-pattern-effects Es)))

(define native-stage-defuns
  [] -> skip
  [D | Ds] -> (do (eval-kl D)
                  (native-stage-defuns Ds)))

(define native-forms->unit
  Fs CT Ps Types -> (let All (native-processed-defines Fs)
                         Ds (native-last-defines All)
                         KL (map (function native-shen->kl) Ds)
                      (do (native-record-defuns KL)
                          (native-stage-defuns KL)
                          (let Is (native-processed-init-kl Fs)
                               Es (native-pattern-effect-kl-forms Fs)
                               Ts (native-type-declaration-forms Types)
                            (do (native-stage-pattern-effects Es)
                                [native-unit KL Is
                                 [[native-compiletime-group
                                   (append Ts CT) Es]]
                                 Ps])))))

(define native-empty-unit
  -> [native-unit [] [] [] []])

(define native-append-units
  [native-unit KL1 Is1 CT1 Ps1]
  [native-unit KL2 Is2 CT2 Ps2]
  -> [native-unit (append KL1 KL2) (append Is1 Is2)
      (append CT1 CT2) (append Ps1 Ps2)])

(define native-last-defuns
  KL -> (reverse (native-last-defuns* (reverse KL) [])))

(define native-last-defuns*
  [] _ -> []
  [[defun F _ _] | KL] Seen -> (native-last-defuns* KL Seen)
    where (element? F Seen)
  [[defun F As B] | KL] Seen
  -> [[defun F As B] | (native-last-defuns* KL [F | Seen])])

(define native-finalize-unit
  [native-unit KL Is CT Ps]
  -> [native-unit (native-last-defuns KL) Is CT Ps])

(define native-unit-kl
  [native-unit KL _ _ _] -> KL)

(define native-unit-packages
  [native-unit _ _ _ Ps] -> Ps)

(define native-arity-form
  [defun F As _] -> [(_scm.prefix-op update-lambda-table) [quote F] (length As)])

(define native-source->unit
  S -> (native-sources->unit [S]))

(define native-sources->unit
  Ss -> (with-native-compiler-state
         (freeze
          (with-native-compiletime-environment
           (freeze (native-source-data->unit (native-read-sources Ss)))))))

(define native-module-sources->unit
  Ss -> (with-native-compiler-state
         (freeze (native-process-module-sources Ss))))

(define native-process-module-sources
  Ss -> (with-native-compiletime-environment
         (freeze
          (native-finalize-unit (native-process-module-sources* Ss)))))

(define native-process-module-sources*
  [] -> (native-empty-unit)
  [S | Ss]
  -> (let U (native-source-data->unit (native-process-module-source S))
          Us (native-process-module-sources* Ss)
       (native-append-units U Us)))

(define native-source-data->unit
  [native-source-data Fs CT Ps Types]
  -> (native-forms->unit Fs CT Ps Types))

)
