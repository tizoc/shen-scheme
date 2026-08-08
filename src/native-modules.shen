\* Copyright (c) 2012-2021 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(package shen-scheme
 [shen.aot.module name mode sources requires exports metadata profile
  module-declaration infer-all runtime compiletime source-kl
  compatible sealed release skip
  update-lambda-table
  scm.shen-scheme-native-key scm.shen-scheme-load-compiled
  scm.shen-scheme-load-compiled-for-compilation
  scm.file-exists? scm.dynamic-wind]

(define native-single-form
  _ [F] -> F
  P [] -> (error "native module declaration ~A is empty~%" P)
  P Fs -> (error "native module declaration ~A expected one top-level form, got ~A~%"
                 P (length Fs)))

(define native-read-module-declaration
  P -> (native-parse-module-declaration
        (native-single-form P (read-file-unprocessed P))))

(define native-parse-module-declaration
  [shen.aot.module | Fs] -> (native-parse-module-fields
                             Fs [] (fail) compatible [] [] infer-all
                             [runtime compiletime] release)
  F -> (error "native module declaration expected shen.aot.module form, got: ~S~%" F))

(define native-add-seen-field
  F Seen -> (if (element? F Seen)
                (error "native module declaration has duplicate field: ~A~%" F)
                [F | Seen]))

(define native-parse-module-fields
  [] _ N M Ss Rs Xs MD P
  -> (native-finalize-module-declaration N M Ss Rs Xs MD P)
  [[name N] | Fs] Seen _ M Ss Rs Xs MD P
  -> (native-parse-module-fields Fs (native-add-seen-field name Seen)
                                 N M Ss Rs Xs MD P)
  [[mode M] | Fs] Seen N _ Ss Rs Xs MD P
  -> (native-parse-module-fields Fs (native-add-seen-field mode Seen)
                                 N M Ss Rs Xs MD P)
  [[sources | Ss] | Fs] Seen N M _ Rs Xs MD P
  -> (native-parse-module-fields Fs (native-add-seen-field sources Seen)
                                 N M Ss Rs Xs MD P)
  [[requires | Rs] | Fs] Seen N M Ss _ Xs MD P
  -> (native-parse-module-fields Fs (native-add-seen-field requires Seen)
                                 N M Ss Rs Xs MD P)
  [[exports infer-all] | Fs] Seen N M Ss Rs _ MD P
  -> (native-parse-module-fields Fs (native-add-seen-field exports Seen)
                                 N M Ss Rs infer-all MD P)
  [[exports | Xs] | Fs] Seen N M Ss Rs _ MD P
  -> (native-parse-module-fields Fs (native-add-seen-field exports Seen)
                                 N M Ss Rs Xs MD P)
  [[metadata | MD] | Fs] Seen N M Ss Rs Xs _ P
  -> (native-parse-module-fields Fs (native-add-seen-field metadata Seen)
                                 N M Ss Rs Xs MD P)
  [[profile P] | Fs] Seen N M Ss Rs Xs MD _
  -> (native-parse-module-fields Fs (native-add-seen-field profile Seen)
                                 N M Ss Rs Xs MD P)
  [F | _] _ _ _ _ _ _ _ _
  -> (error "native module declaration has unknown or malformed field: ~S~%" F))

(define native-finalize-module-declaration
  N M Ss Rs Xs MD P
  -> (error "native module declaration requires a name field~%")
    where (= N (fail))
  N M Ss Rs Xs MD P
  -> [module-declaration (native-module-symbol name N) (native-compile-mode M)
      (native-source-list Ss) (native-symbol-list requires Rs)
      (native-exports Xs) (native-metadata-list MD)
      (native-compile-profile P)])

(define native-module-symbol
  _ X -> X where (symbol? X)
  F X -> (error "native module declaration field ~A expected a symbol, got: ~S~%" F X))

(define native-source-list
  [] -> (error "native module declaration requires at least one source~%")
  Ss -> Ss where (native-string-list? Ss)
  Ss -> (error "native module declaration sources must be strings, got: ~S~%" Ss))

(define native-string-list?
  [] -> true
  [S | Ss] -> (and (string? S) (native-string-list? Ss))
  _ -> false)

(define native-symbol-list
  _ [] -> []
  F [X | Xs] -> [X | (native-symbol-list F Xs)] where (symbol? X)
  F Xs -> (error "native module declaration field ~A expected symbols, got: ~S~%" F Xs))

(define native-exports
  infer-all -> infer-all
  Xs -> (native-symbol-list exports Xs))

(define native-metadata-list
  [] -> []
  [M | Ms] -> [(native-metadata M) | (native-metadata-list Ms)]
  MD -> (error "native module declaration metadata must be symbols, got: ~S~%" MD))

(define native-metadata
  runtime -> runtime
  compiletime -> compiletime
  source-kl -> source-kl
  M -> (error "native module declaration expected metadata runtime, compiletime, or source-kl, got: ~S~%" M))

(define native-module-declaration-name
  [module-declaration N _ _ _ _ _ _] -> N)

(define native-module-declaration-mode
  [module-declaration _ M _ _ _ _ _] -> M)

(define native-module-declaration-sources
  [module-declaration _ _ Ss _ _ _ _] -> Ss)

(define native-module-declaration-requires
  [module-declaration _ _ _ Rs _ _ _] -> Rs)

(define native-module-declaration-exports
  [module-declaration _ _ _ _ Xs _ _] -> Xs)

(define native-module-declaration-metadata
  [module-declaration _ _ _ _ _ MD _] -> MD)

(define native-module-declaration-profile
  [module-declaration _ _ _ _ _ _ P] -> P)

(define native-module-declaration-key
  [module-declaration N M Ss Rs Xs MD P]
  -> ((foreign scm.shen-scheme-native-key) Ss [N M Rs Xs MD P]))

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
          (native-sources->unit (native-module-declaration-sources D))
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

(define native-sources->unit/with-arities
  Ss As -> (with-native-compiler-state
            (freeze (native-sources->unit/with-arities* Ss As))))

(define native-sources->unit/with-arities*
  Ss As -> (let X (native-expand-forms (native-read-source-forms Ss))
                O (value *property-vector*)
                N (native-copy-property-vector O)
             ((foreign scm.dynamic-wind)
              (freeze (set *property-vector* N))
              (freeze
               (do (native-register-arities As)
                   (native-source-data->unit (native-process-expanded X))))
              (freeze (set *property-vector* O)))))

(define native-defun-arity
  [defun F As _] -> [F (length As)])

(define native-exported-arities
  KL Xs -> (map (function native-defun-arity)
                (native-exported-kl KL Xs)))

(define native-prepare-module
  compatible _ Xs -> (error "native compiler explicit exports require sealed mode, got: ~S~%" Xs)
    where (not (= Xs infer-all))
  _ Ss Xs
  -> (let KL (native-unit-kl (native-sources->unit/with-arities Ss []))
          CXs (native-validate-exports Xs (native-local-map KL))
       (native-register-arities (native-exported-arities KL CXs))))

(define native-prepare-module/declaration*
  [module-declaration N M Ss Rs Xs _ _] Dir Stack L
  -> (if (element? N Stack)
         (native-cycle-error N)
         (if (element? N L)
             (do (native-prepare-module M Ss Xs) L)
             (let L (native-prepare-module-requirements
                      Rs Dir [N | Stack] L)
               (do (native-prepare-module M Ss Xs)
                   [N | L])))))

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
  [module-declaration M _ _ Rs _ _ _] Dir Stack L
  -> (if (element? M Stack)
         (native-cycle-error M)
         (if (element? M L)
             (let O (native-module-object-path Dir M)
                  O (native-require-existing-file O "module object")
               (do (Rd O) L))
             (let L (native-load-module-requirements/with
                      Ld Rd Rs Dir [M | Stack] L)
                  O (native-module-object-path Dir M)
                  O (native-require-existing-file O "module object")
               (do (Ld O) [M | L])))))

)
