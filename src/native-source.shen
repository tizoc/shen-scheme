\* Copyright (c) 2012-2021 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(package shen-scheme
 [release debug wpo unsafe
  define defun defprolog declare defmacro datatype synonyms package
  native-unit runtime compiletime source-kl quote eval update-lambda-table
  _scm.prefix-op scm.dynamic-wind scm.hashtable-copy]

(set *native-compiler-state-depth* 0)

(define native-copy-property-vector
  D -> ((foreign scm.hashtable-copy) D true))

(define with-native-compiler-state
  F -> (F) where (> (value *native-compiler-state-depth*) 0)
  F -> (let Old (value *property-vector*)
            New (native-copy-property-vector Old)
            Macros (value *macros*)
            SigF (value shen.*sigf*)
            Synonyms (value shen.*synonyms*)
            Demod (value shen.*demodulation-function*)
            Datatypes (value shen.*datatypes*)
            AllDatatypes (value shen.*alldatatypes*)
            UserDefs (value shen.*userdefs*)
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
               (set *native-compiler-state-depth* 0))))))

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
  F -> (let Bs (read-file-as-bytelist F)
            Fs (trap-error (compile (/. X (shen.<s-exprs> X)) Bs)
                           (/. E (shen.reader-error (value shen.*residue*))))
         Fs))

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
           [native-source-data
            (map (/. X (shen.process-applications X Ts)) Xs)
            (mapcan (/. X X) CTs)
            Ps])))

(define native-read-source
  S -> (native-read-sources [S]))

(define native-read-sources
  Ss -> (native-process-expanded (native-expand-forms (native-read-source-forms Ss))))

(define native-read-source-forms
  [] -> []
  [S | Ss] -> (append (read-file-unprocessed S) (native-read-source-forms Ss)))

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

(define native-forms->unit
  Fs CT Ps -> (let All (native-processed-defines Fs)
                   Ds (native-last-defines All)
                   KL (map (function native-shen->kl) Ds)
                   Is (map (function native-init->kl) (native-processed-inits Fs))
                   Ts (native-type-declaration-forms (native-type-table All))
                [native-unit KL Is (append Ts CT)
                 Ps]))

(define native-unit-kl
  [native-unit KL _ _ _] -> KL)

(define native-arity-form
  [defun F As _] -> [(_scm.prefix-op update-lambda-table) [quote F] (length As)])

(define native-source->unit
  S -> (native-sources->unit [S]))

(define native-sources->unit
  Ss -> (with-native-compiler-state
         (freeze (native-source-data->unit (native-read-sources Ss)))))

(define native-source-data->unit
  [native-source-data Fs CT Ps] -> (native-forms->unit Fs CT Ps))

)
