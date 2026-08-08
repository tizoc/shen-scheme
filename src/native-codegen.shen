\* Copyright (c) 2012-2021 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(package shen-scheme
 [compatible sealed infer-all runtime compiletime source-kl
  native-unit defun define quote module import if begin define-top-level-value skip
  update-lambda-table install-runtime! install-compiletime! install-init! loaded
  shen-scheme-native-load-init?
  _scm.prefix-op _scm.kl->scheme _scm.with-native-context
  scm.shen-scheme-native-key]

(define native-scheme-forms
  S -> (native-scheme-forms/mode S compatible))

(define native-scheme-forms/mode
  S M -> (let M (native-compile-mode M)
           (native-scheme-forms* (native-module-name/mode S M)
                                 (native-source->unit S)
                                 M infer-all [runtime compiletime])))

(define native-scheme-forms*
  _ U compatible infer-all MD -> (native-compatible-scheme-forms U MD)
  _ _ compatible Xs _ -> (error "native compiler explicit exports require sealed mode, got: ~S~%" Xs)
  M U sealed Xs MD -> (native-sealed-scheme-forms* M U Xs MD))

(define native-compile-kl-forms
  M LM KL -> (_scm.with-native-context
              M LM (freeze (map (function _scm.kl->scheme) KL))))

(define native-compatible-scheme-forms
  [native-unit KL Is CT Ps] MD
  -> (let Ds (native-compile-kl-forms compatible [] KL)
          RT (native-runtime-metadata-forms KL MD)
          Meta (native-compiletime-metadata-forms KL CT MD)
          Init (native-compile-kl-forms compatible [] Is)
       (append Ds RT Ps Meta
               [(native-load-init-form (append Init [[quote loaded]]))])))

(define native-compile-mode
  compatible -> compatible
  sealed -> sealed
  "compatible" -> compatible
  "sealed" -> sealed
  M -> (error "native compiler expected mode compatible or sealed, got: ~S~%" M))

(define native-local-map
  KL -> (native-local-map* KL 0))

(define native-local-map*
  [] _ -> []
  [[defun F _ _] | KL] N -> [[F (native-local-symbol N)] |
                             (native-local-map* KL (+ N 1))])

(define native-local-symbol
  N -> (intern (@s "shen_native_" (str N))))

(define native-module-name
  S -> (intern (@s "shen_native_unit_"
                  ((foreign scm.shen-scheme-native-key) [S] [sealed]))))

(define native-module-name/mode
  _ compatible -> skip
  S sealed -> (native-module-name S))

(define native-export-form
  [F L] -> [define-top-level-value [quote (_scm.prefix-op F)] L])

(define native-export-forms
  Ms -> (map (function native-export-form) Ms))

(define native-mapping-name
  [F _] -> F)

(define native-mapping-local
  [_ L] -> L)

(define native-runtime-metadata-forms
  KL MD -> (if (element? runtime MD) (map (function native-arity-form) KL) []))

(define native-source-kl-form
  [defun F As B] -> [(_scm.prefix-op shen.record-kl) [quote F]
                     [quote [defun F As B]]])

(define native-source-kl-forms
  KL MD -> (if (element? source-kl MD)
               (map (function native-source-kl-form) KL)
               []))

(define native-compiletime-metadata-forms
  KL CT MD
  -> (append (if (element? compiletime MD)
                     CT
                     [])
                  (native-source-kl-forms KL MD)))

(define native-exported-local-map
  LM infer-all -> LM
  [] _ -> []
  [[F L] | Ms] Xs -> (if (element? F Xs)
                         [[F L] | (native-exported-local-map Ms Xs)]
                         (native-exported-local-map Ms Xs)))

(define native-exported-kl
  KL infer-all -> KL
  [] _ -> []
  [[defun F As B] | KL] Xs -> (if (element? F Xs)
                                  [[defun F As B] | (native-exported-kl KL Xs)]
                                  (native-exported-kl KL Xs)))

(define native-validate-exports
  infer-all _ -> infer-all
  [] _ -> []
  [X | Xs] LM -> [X | (native-validate-exports Xs LM)]
    where (element? X (map (function native-mapping-name) LM))
  [X | _] _ -> (error "native module declaration exports unknown function: ~S~%" X))

(define native-module-runtime-forms
  KL LM Xs MD Ps
  -> (append (if (element? runtime MD)
                 (append (native-export-forms
                          (native-exported-local-map LM Xs))
                         (map (function native-arity-form)
                              (native-exported-kl KL Xs)))
                 [])
             Ps [[quote loaded]]))

(define native-module-compiletime-forms
  KL CT MD
  -> (append (native-compiletime-metadata-forms KL CT MD) [[quote loaded]]))

(define native-module-init-forms
  Is -> (append Is [[quote loaded]]))

(define native-load-init-form
  Is -> [if [shen-scheme-native-load-init?] [begin | Is] [quote loaded]])

(define native-sealed-module-form
  M Ds RT CT Is
  -> [module M [install-runtime! install-compiletime! install-init!]
                 | (append Ds [[define [install-runtime!] | RT]
                               [define [install-compiletime!] | CT]
                               [define [install-init!] | Is]])])

(define native-sealed-scheme-forms
  S -> (native-sealed-scheme-forms* (native-module-name S) (native-source->unit S)
                                    infer-all [runtime compiletime]))

(define native-sealed-scheme-forms*
  M [native-unit KL Is CT Ps] Xs MD
  -> (let LM (native-local-map KL)
          CXs (native-validate-exports Xs LM)
          Ds (native-compile-kl-forms sealed LM KL)
          Init (native-compile-kl-forms sealed LM Is)
          RT (native-module-runtime-forms KL LM CXs MD Ps)
          Meta (native-module-compiletime-forms KL CT MD)
          Run (native-module-init-forms Init)
       [(native-sealed-module-form M Ds RT Meta Run)
        [import M] [install-runtime!] [install-compiletime!]
        (native-load-init-form [[install-init!]])]))

)
