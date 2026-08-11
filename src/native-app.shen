\* Copyright (c) 2012-2021 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(package shen-scheme
 [app native-unit module-declaration module-app-map runtime infer-all
  defun define quote module loaded
  scm.shen-scheme-native-key]

(define native-app-sources
  M Ms -> (append Ms [M]))

(define native-app-key
  Ss Os WPO? -> ((foreign scm.shen-scheme-native-key) Ss [Os WPO?]))

(define native-app-name-string
  K -> (make-string "app_~A" K))

(define native-app-root
  Dir K -> (make-string "~A/~A" Dir K))

(define native-app-module-local-symbol
  MI FI -> (intern (make-string "shen_native_m~A_f~A" MI FI)))

(define native-app-local-map
  KL MI -> (native-app-local-map* KL MI 0))

(define native-app-local-map*
  [] _ _ -> []
  [[defun F _ _] | KL] MI FI
  -> [[F (native-app-module-local-symbol MI FI)] |
      (native-app-local-map* KL MI (+ FI 1))])

(define native-app-flatten-local-maps
  [] -> []
  [M | Ms] -> (append M (native-app-flatten-local-maps Ms)))

(define native-app-module-name
  App MI -> (intern (make-string "shen_native_~A_module~A" App MI)))

(define native-app-runtime-installer
  MI -> (intern (make-string "install-runtime-~A!" MI)))

(define native-app-compiletime-installer
  MI -> (intern (make-string "install-compiletime-~A!" MI)))

(define native-app-init-installer
  MI -> (intern (make-string "install-init-~A!" MI)))

(define native-app-exports
  LM RI CI II -> (append (map (function native-mapping-local) LM) [RI CI II]))

(define native-app-module-form
  App MI [native-unit KL Is _ Ps] LM PMs
  -> (let M (native-app-module-name App MI)
          RI (native-app-runtime-installer MI)
          CI (native-app-compiletime-installer MI)
          II (native-app-init-installer MI)
          VM (append LM (native-app-flatten-local-maps PMs))
          Ds (native-compile-kl-forms app VM KL)
          Init (native-compile-kl-forms app VM Is)
          RT (native-module-runtime-forms KL LM infer-all [runtime] Ps)
          Run (native-module-init-forms Init)
       [module M (native-app-exports LM RI CI II)
        | (append Ds [[define [RI] | RT]
                      [define [CI] [quote loaded]]
                      [define [II] | Run]])]))

(define native-app-module-forms*
  [] _ _ _ -> []
  [S | Ss] App MI Ms
  -> (let U (native-source->unit S)
          KL (native-unit-kl U)
          LM (native-app-local-map KL MI)
       [(native-app-module-form App MI U LM Ms) |
        (native-app-module-forms* Ss App (+ MI 1) [LM | Ms])]))

(define native-app-module-forms
  Ss App -> (with-native-compiler-state
             (freeze (native-app-module-forms* Ss App 0 []))))

(define native-app-install-forms*
  N N -> []
  I N -> [[(native-app-runtime-installer I)]
          [(native-app-compiletime-installer I)]
          [(native-app-init-installer I)]
          | (native-app-install-forms* (+ I 1) N)])

(define native-app-install-forms
  N -> (native-app-install-forms* 0 N))

(define native-module-app-result-loaded
  [L _] -> L)

(define native-module-app-result-declarations
  [_ Ds] -> Ds)

(define native-module-app-declarations
  D Dir -> (reverse (native-module-app-result-declarations
                     (native-module-app-declarations* D Dir [] [] []))))

(define native-module-app-declarations*
  D Dir Stack L Ds
  -> (let M (native-module-declaration-name D)
       (if (element? M Stack)
           (native-cycle-error M)
           (if (element? M L)
               [L Ds]
               (let R (native-module-app-requirements
                        (native-module-declaration-requires D)
                        Dir [M | Stack] L Ds)
                    L (native-module-app-result-loaded R)
                    Ds (native-module-app-result-declarations R)
                 [[M | L] [D | Ds]])))))

(define native-module-app-requirements
  [] _ _ L Ds -> [L Ds]
  [M | Ms] Dir Stack L Ds
  -> (let D (native-read-required-module-declaration M Dir)
          R (native-module-app-declarations* D Dir Stack L Ds)
       (native-module-app-requirements
        Ms Dir Stack
        (native-module-app-result-loaded R)
        (native-module-app-result-declarations R))))

(define native-module-app-sources
  [] -> []
  [D | Ds]
  -> (append (native-module-declaration-sources D)
             (native-module-app-sources Ds)))

(define native-module-app-key
  Ds Os WPO? -> ((foreign scm.shen-scheme-native-key)
                 (native-module-app-sources Ds)
                 [(map (function native-module-declaration-key) Ds) Os WPO?]))

(define native-module-app-map-exports
  [module-app-map _ _ Xs _] -> Xs)

(define native-module-app-map-arities
  [module-app-map _ _ _ As] -> As)

(define native-module-app-find-map
  M [] -> (error "native module app missing required module: ~A~%" M)
  M [[module-app-map N I Xs As] | Ms]
  -> (if (= M N)
         [module-app-map N I Xs As]
         (native-module-app-find-map M Ms)))

(define native-module-app-required-visible-map
  [] _ -> []
  [R | Rs] Ms
  -> (append (native-module-app-required-visible-map Rs Ms)
             (native-module-app-map-exports (native-module-app-find-map R Ms))))

(define native-module-app-required-visible-arities
  [] _ -> []
  [R | Rs] Ms
  -> (append
      (native-module-app-required-visible-arities Rs Ms)
      (native-module-app-map-arities (native-module-app-find-map R Ms))))

(define native-module-app-export-owner
  _ [] -> []
  F [[F R] | _] -> [R]
  F [_ | Xs] -> (native-module-app-export-owner F Xs))

(define native-module-app-record-exports
  _ [] Seen -> Seen
  R [[F _] | Xs] Seen
  -> (let Owner (native-module-app-export-owner F Seen)
       (if (= [] Owner)
           (native-module-app-record-exports R Xs [[F R] | Seen])
           (error "native module app has ambiguous exported function ~A in direct requirements ~A and ~A~%"
                  F (hd Owner) R))))

(define native-module-app-validate-required-exports
  Rs Ms -> (native-module-app-validate-required-exports* Rs Ms []))

(define native-module-app-validate-required-exports*
  [] _ _ -> skip
  [R | Rs] Ms Seen
  -> (let Xs (native-module-app-map-exports
              (native-module-app-find-map R Ms))
          Seen (native-module-app-record-exports R Xs Seen)
       (native-module-app-validate-required-exports* Rs Ms Seen)))

(define native-module-app-module-form
  App I Xs MD [native-unit KL Is CT Ps] LM XM RM
  -> (let M (native-app-module-name App I)
          RI (native-app-runtime-installer I)
          CI (native-app-compiletime-installer I)
          II (native-app-init-installer I)
          VM (append LM RM)
          Ds (native-compile-kl-forms app VM KL)
          Init (native-compile-kl-forms app VM Is)
          RT (native-module-runtime-forms KL LM Xs MD Ps)
          Meta (native-module-compiletime-forms KL CT MD)
          Run (native-module-init-forms Init)
       [module M (native-app-exports XM RI CI II)
        | (append Ds [[define [RI] | RT]
                      [define [CI] | Meta]
                      [define [II] | Run]])]))

(define native-module-app-module-forms-result-forms
  [_ Fs] -> Fs)

(define native-module-app-module-forms
  Ds App -> (with-native-compiler-state
             (freeze
              (reverse (native-module-app-module-forms-result-forms
                        (native-module-app-module-forms*
                         Ds App 0 [] []))))))

(define native-module-app-module-forms*
  [] _ _ Ms Fs -> [Ms Fs]
  [D | Ds] App I Ms Fs
  -> (let N (native-module-declaration-name D)
          Ss (native-module-declaration-source-specs D)
          Rs (native-module-declaration-requires D)
          Xs (native-module-declaration-exports D)
          MD (native-module-declaration-metadata D)
       (do (native-module-app-validate-required-exports Rs Ms)
           (let RM (native-module-app-required-visible-map Rs Ms)
                As (native-module-app-required-visible-arities Rs Ms)
                U (native-module-sources->unit/with-arities Ss As)
                KL (native-unit-kl U)
                LM (native-app-local-map KL I)
                CXs (native-validate-exports Xs LM)
                XM (native-exported-local-map LM CXs)
                AM (native-exported-arities KL CXs)
                F (native-module-app-module-form App I CXs MD U LM XM RM)
                MM [module-app-map N I XM AM]
             (native-module-app-module-forms*
              Ds App (+ I 1) [MM | Ms] [F | Fs])))))

)
