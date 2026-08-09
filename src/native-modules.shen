\* Copyright (c) 2012-2021 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(package shen-scheme
 [shen.module version name sources requires requires-features extension
  shen/scheme mode exports metadata profile tc+ tc-
  module-declaration infer-all runtime compiletime source-kl
  compatible sealed release skip
  quote update-lambda-table _scm.prefix-op
  scm.shen-scheme-native-key scm.shen-scheme-load-compiled
  scm.shen-scheme-load-compiled-for-compilation
  scm.shen-scheme-resolve-module-source
  scm.file-exists? scm.dynamic-wind]

(define native-single-form
  _ [F] -> F
  P [] -> (error "native module declaration ~A is empty~%" P)
  P Fs -> (error "native module declaration ~A expected one top-level form, got ~A~%"
                 P (length Fs)))

(define native-read-module-declaration
  P -> (native-resolve-module-declaration
        P
        (native-parse-module-declaration
         (native-single-form P (read-file-unprocessed P)))))

(define native-parse-module-declaration
  [shen.module | Fs] -> (native-parse-module-fields
                         Fs [] (fail) (fail) (fail) [] [] [])
  F -> (error "module declaration expected shen.module form, got: ~S~%" F))

(define native-add-seen-field
  F Seen -> (if (element? F Seen)
                (error "module declaration has duplicate field: ~A~%" F)
                [F | Seen]))

(define native-parse-module-fields
  [] _ V N Ss Rs RFs Es
  -> (native-finalize-module-declaration V N Ss Rs RFs Es)
  [[version V] | Fs] Seen _ N Ss Rs RFs Es
  -> (native-parse-module-fields Fs (native-add-seen-field version Seen)
                                 V N Ss Rs RFs Es)
  [[name N] | Fs] Seen V _ Ss Rs RFs Es
  -> (native-parse-module-fields Fs (native-add-seen-field name Seen)
                                 V N Ss Rs RFs Es)
  [[sources | Ss] | Fs] Seen V N _ Rs RFs Es
  -> (native-parse-module-fields Fs (native-add-seen-field sources Seen)
                                 V N Ss Rs RFs Es)
  [[requires | Rs] | Fs] Seen V N Ss _ RFs Es
  -> (native-parse-module-fields Fs (native-add-seen-field requires Seen)
                                 V N Ss Rs RFs Es)
  [[requires-features | RFs] | Fs] Seen V N Ss Rs _ Es
  -> (native-parse-module-fields
      Fs (native-add-seen-field requires-features Seen)
      V N Ss Rs RFs Es)
  [[extension Id | Xs] | Fs] Seen V N Ss Rs RFs Es
  -> (native-parse-module-fields
      Fs Seen V N Ss Rs RFs
      (native-add-module-extension
       (native-parse-module-extension Id Xs) Es))
  [F | _] _ _ _ _ _ _ _
  -> (error "module declaration has unknown or malformed field: ~S~%" F))

(define native-finalize-module-declaration
  V N Ss Rs RFs Es
  -> (error "module declaration requires (version 1)~%")
    where (not (= V 1))
  V N Ss Rs RFs Es
  -> (error "module declaration requires a name field~%")
    where (= N (fail))
  V N Ss Rs RFs Es
  -> (error "module declaration requires a sources field~%")
    where (= Ss (fail))
  V N Ss Rs RFs Es
  -> (let RFs (native-symbol-list requires-features RFs)
       (do (native-require-features RFs)
           [module-declaration
            (native-module-symbol name N)
            (native-source-list Ss)
            (native-symbol-list requires Rs)
            RFs
            (native-finalize-module-extensions (reverse Es))])))

(define native-module-symbol
  _ X -> X where (symbol? X)
  F X -> (error "module declaration field ~A expected a symbol, got: ~S~%" F X))

(define native-source-list
  Ss -> (native-source-list* Ss (fail) [] false))

(define native-source-list*
  [] _ [] _ -> (error "module declaration requires at least one source~%")
  [] M _ true
  -> (error "module declaration source mode ~A must be followed by a source~%" M)
  [] _ Ss false -> (reverse Ss)
  [tc+ | _] M _ true
  -> (error "module declaration source mode ~A must be followed by a source~%" M)
  [tc- | _] M _ true
  -> (error "module declaration source mode ~A must be followed by a source~%" M)
  [tc+ | Ss] _ Out false -> (native-source-list* Ss tc+ Out true)
  [tc- | Ss] _ Out false -> (native-source-list* Ss tc- Out true)
  [S | Ss] M Out _
  -> (error "module declaration source ~S must follow tc+ or tc-~%" S)
    where (= M (fail))
  [S | Ss] M Out _
  -> (native-source-list* Ss M [[module-source M S] | Out] false)
    where (string? S)
  [S | _] _ _ _
  -> (error "module declaration source must be a string, got: ~S~%" S))

(define native-symbol-list
  _ [] -> []
  F [X | Xs] -> [X | (native-symbol-list F Xs)] where (symbol? X)
  F Xs -> (error "module declaration field ~A expected symbols, got: ~S~%" F Xs))

(define native-require-features
  [] -> skip
  [F | Fs] -> (if (element? F (shen.x.features.current))
                  (native-require-features Fs)
                  (error "module declaration requires unavailable feature: ~A~%" F)))

(define native-exports
  infer-all -> infer-all
  Xs -> (native-symbol-list exports Xs))

(define native-metadata-list
  [] -> []
  [M | Ms] -> [(native-metadata M) | (native-metadata-list Ms)]
  MD -> (error "shen/scheme extension metadata must be symbols, got: ~S~%" MD))

(define native-metadata
  runtime -> runtime
  compiletime -> compiletime
  source-kl -> source-kl
  M -> (error "shen/scheme extension expected metadata runtime, compiletime, or source-kl, got: ~S~%" M))

(define native-parse-module-extension
  shen/scheme Fs -> [module-extension shen/scheme
                     (native-parse-shen-scheme-extension
                      Fs [] compatible infer-all
                      [runtime compiletime] release)]
  Id Fs -> [module-extension (native-module-symbol extension Id) Fs])

(define native-parse-shen-scheme-extension
  [] _ M Xs MD P -> [shen-scheme-extension M Xs MD P]
  [[mode M] | Fs] Seen _ Xs MD P
  -> (native-parse-shen-scheme-extension
      Fs (native-add-seen-field mode Seen)
      (native-compile-mode M) Xs MD P)
  [[exports infer-all] | Fs] Seen M _ MD P
  -> (native-parse-shen-scheme-extension
      Fs (native-add-seen-field exports Seen)
      M infer-all MD P)
  [[exports | Xs] | Fs] Seen M _ MD P
  -> (native-parse-shen-scheme-extension
      Fs (native-add-seen-field exports Seen)
      M (native-exports Xs) MD P)
  [[metadata | MD] | Fs] Seen M Xs _ P
  -> (native-parse-shen-scheme-extension
      Fs (native-add-seen-field metadata Seen)
      M Xs (native-metadata-list MD) P)
  [[profile P] | Fs] Seen M Xs MD _
  -> (native-parse-shen-scheme-extension
      Fs (native-add-seen-field profile Seen)
      M Xs MD (native-compile-profile P))
  [F | _] _ _ _ _ _
  -> (error "shen/scheme extension has unknown or malformed field: ~S~%" F))

(define native-module-extension-id
  [module-extension Id _] -> Id)

(define native-module-extension-ids
  Es -> (map (function native-module-extension-id) Es))

(define native-add-module-extension
  [module-extension Id X] Es
  -> (if (element? Id (native-module-extension-ids Es))
         (error "module declaration has duplicate extension: ~A~%" Id)
         [[module-extension Id X] | Es]))

(define native-default-shen-scheme-extension
  -> [module-extension shen/scheme
      [shen-scheme-extension compatible infer-all
       [runtime compiletime] release]])

(define native-finalize-module-extensions
  Es -> Es where (element? shen/scheme (native-module-extension-ids Es))
  Es -> (append Es [(native-default-shen-scheme-extension)]))

(define native-resolve-module-source-path
  D S -> ((foreign scm.shen-scheme-resolve-module-source) D S))

(define native-resolve-module-source
  D [module-source M S]
  -> [module-source M (native-resolve-module-source-path D S)])

(define native-resolve-module-declaration
  D [module-declaration N Ss Rs Fs Es]
  -> [module-declaration
      N
      (map (/. S (native-resolve-module-source D S)) Ss)
      Rs Fs Es])

(define native-module-declaration-name
  [module-declaration N _ _ _ _] -> N)

(define native-module-declaration-source-specs
  [module-declaration _ Ss _ _ _] -> Ss)

(define native-module-declaration-sources
  D -> (map (function native-module-source-path)
            (native-module-declaration-source-specs D)))

(define native-module-declaration-source-modes
  D -> (map (function native-module-source-mode)
            (native-module-declaration-source-specs D)))

(define native-module-declaration-requires
  [module-declaration _ _ Rs _ _] -> Rs)

(define native-module-declaration-required-features
  [module-declaration _ _ _ Fs _] -> Fs)

(define native-module-declaration-extensions
  [module-declaration _ _ _ _ Es] -> Es)

(define native-module-extension
  Id [[module-extension Id X] | _] -> X
  Id [_ | Es] -> (native-module-extension Id Es)
  _ [] -> (fail))

(define native-module-declaration-extension
  Id D -> (native-module-extension
           Id (native-module-declaration-extensions D)))

(define native-module-declaration-shen-scheme-extension
  D -> (native-module-declaration-extension shen/scheme D))

(define native-module-declaration-mode
  D -> (native-shen-scheme-extension-mode
        (native-module-declaration-shen-scheme-extension D)))

(define native-module-declaration-exports
  D -> (native-shen-scheme-extension-exports
        (native-module-declaration-shen-scheme-extension D)))

(define native-module-declaration-metadata
  D -> (native-shen-scheme-extension-metadata
        (native-module-declaration-shen-scheme-extension D)))

(define native-module-declaration-profile
  D -> (native-shen-scheme-extension-profile
        (native-module-declaration-shen-scheme-extension D)))

(define native-shen-scheme-extension-mode
  [shen-scheme-extension M _ _ _] -> M)

(define native-shen-scheme-extension-exports
  [shen-scheme-extension _ Xs _ _] -> Xs)

(define native-shen-scheme-extension-metadata
  [shen-scheme-extension _ _ MD _] -> MD)

(define native-shen-scheme-extension-profile
  [shen-scheme-extension _ _ _ P] -> P)

(define native-module-declaration-key
  D -> ((foreign scm.shen-scheme-native-key)
        (native-module-declaration-sources D)
        [(native-module-declaration-name D)
         (native-module-declaration-source-modes D)
         (native-module-declaration-requires D)
         (native-module-declaration-required-features D)
         (native-module-declaration-mode D)
         (native-module-declaration-exports D)
         (native-module-declaration-metadata D)
         (native-module-declaration-profile D)]))

(define native-module-declaration-module-name
  D -> (intern (make-string "shen_native_decl_~A"
                            (native-module-declaration-key D))))

(define native-module-declaration-module-name/mode
  _ compatible -> skip
  D sealed -> (native-module-declaration-module-name D))

(define native-module-declaration-scheme-forms
  D -> (let M (native-module-declaration-mode D)
         (native-scheme-forms*
          (native-module-declaration-module-name/mode D M)
          (native-module-sources->unit
           (native-module-declaration-source-specs D))
          M
          (native-module-declaration-exports D)
          (native-module-declaration-metadata D))))

(define native-module-object-path
  Dir M -> (make-string "~A/~A.so" Dir M))

(define native-module-declaration-path
  Dir M -> (make-string "~A/~A.shenmod" Dir M))

(define load-compiled
  O -> ((foreign scm.shen-scheme-load-compiled) O))

(define native-load-compiled-for-compilation
  O -> ((foreign scm.shen-scheme-load-compiled-for-compilation) O))

(define native-require-existing-file
  F _ -> F where ((foreign scm.file-exists?) F)
  F D -> (error "native module expected ~A to exist: ~A~%" D F))

(define native-require-module-dir
  [] _ -> skip
  _ Dir -> (if (= Dir (fail))
               (error "native module requires need a module directory~%")
               skip))

(define native-cycle-error
  M -> (error "native module dependency cycle includes: ~A~%" M))

(define native-load-module-requirements
  Rs Dir Stack L -> (native-load-module-requirements/with
                     (function load-compiled)
                     (function native-load-compiled-for-compilation)
                     Rs Dir Stack L))

(define native-prepare-module-requirements
  Rs Dir Stack L
  -> (do (native-require-module-dir Rs Dir)
         (native-prepare-module-requirements* Rs Dir Stack L)))

(define native-prepare-module-requirements*
  [] _ _ L -> L
  [R | Rs] Dir Stack L
  -> (let P (native-module-declaration-path Dir R)
          P (native-require-existing-file P "required module declaration")
          D (native-read-module-declaration P)
          L (native-prepare-module/declaration* D Dir Stack L)
       (native-prepare-module-requirements* Rs Dir Stack L)))

(define native-register-arities
  [] -> skip
  [[F A] | As] -> (do (native-register-arities As)
                       (update-lambda-table F A)))

(define native-record-package-form
  [F [quote N] [quote Xs]] -> (shen.record-external N Xs)
    where (= F (_scm.prefix-op shen.record-external))
  [F [quote N] [quote Xs] [quote Fs]] -> (shen.record-internal N Xs Fs)
    where (= F (_scm.prefix-op shen.record-internal)))

(define native-record-package-forms
  [] -> skip
  [F | Fs] -> (do (native-record-package-form F)
                  (native-record-package-forms Fs)))

(define native-module-sources->unit/with-arities
  Ss As -> (with-native-compiler-state
            (freeze (native-module-sources->unit/with-arities* Ss As))))

(define native-module-sources->unit/with-arities*
  Ss As -> (let O (value *property-vector*)
                N (native-copy-property-vector O)
                U ((foreign scm.dynamic-wind)
                   (freeze (set *property-vector* N))
                   (freeze
                    (do (native-register-arities As)
                        (native-source-data->unit
                         (native-process-module-sources Ss))))
                   (freeze (set *property-vector* O)))
             (do (native-record-package-forms (native-unit-packages U))
                 U)))

(define native-defun-arity
  [defun F As _] -> [F (length As)])

(define native-exported-arities
  KL Xs -> (map (function native-defun-arity)
                (native-exported-kl KL Xs)))

(define native-prepare-module
  compatible _ Xs -> (error "native compiler explicit exports require sealed mode, got: ~S~%" Xs)
    where (not (= Xs infer-all))
  _ Ss Xs
  -> (let KL (native-unit-kl
              (native-module-sources->unit/with-arities Ss []))
          CXs (native-validate-exports Xs (native-local-map KL))
       (native-register-arities (native-exported-arities KL CXs))))

(define native-prepare-module/declaration*
  D Dir Stack L
  -> (let N (native-module-declaration-name D)
          M (native-module-declaration-mode D)
          Ss (native-module-declaration-source-specs D)
          Rs (native-module-declaration-requires D)
          Xs (native-module-declaration-exports D)
       (if (element? N Stack)
           (native-cycle-error N)
           (if (element? N L)
               (do (native-prepare-module M Ss Xs) L)
               (let L (native-prepare-module-requirements
                        Rs Dir [N | Stack] L)
                 (do (native-prepare-module M Ss Xs)
                     [N | L]))))))

(define native-load-module-requirements/with
  Ld Rd Rs Dir Stack L
  -> (do (native-require-module-dir Rs Dir)
         (native-load-module-requirements*/with Ld Rd Rs Dir Stack L)))

(define native-load-module-requirements*/with
  _ _ [] _ _ L -> L
  Ld Rd [R | Rs] Dir Stack L
  -> (let P (native-module-declaration-path Dir R)
          P (native-require-existing-file P "required module declaration")
          D (native-read-module-declaration P)
          L (load-module/declaration*/with Ld Rd D Dir Stack L)
       (native-load-module-requirements*/with Ld Rd Rs Dir Stack L)))

(define load-module
  F Dir -> (load-module/declaration (native-read-module-declaration F) Dir))

(define load-module/declaration
  D Dir -> (load-module/declaration* D Dir [] []))

(define load-module/declaration*
  D Dir Stack L -> (load-module/declaration*/with
                    (function load-compiled)
                    (function native-load-compiled-for-compilation)
                    D Dir Stack L))

(define load-module/declaration*/with
  Ld Rd
  D Dir Stack L
  -> (let M (native-module-declaration-name D)
          Rs (native-module-declaration-requires D)
       (if (element? M Stack)
           (native-cycle-error M)
           (if (element? M L)
               (let O (native-module-object-path Dir M)
                    O (native-require-existing-file O "module object")
                 (do (Rd O) L))
               (let L (native-load-module-requirements/with
                        Ld Rd Rs Dir [M | Stack] L)
                    O (native-module-object-path Dir M)
                    O (native-require-existing-file O "module object")
                 (do (Ld O) [M | L]))))))

)
