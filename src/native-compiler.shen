\* Copyright (c) 2012-2021 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(package shen-scheme
 [compatible
  scm.shen-scheme-compile-native-app
  scm.shen-scheme-compile-native-forms-direct
  scm.shen-scheme-compile-native-forms
  scm.shen-scheme-delete-file-if-exists
  scm.file-exists?]

(define native-compile-forms/direct
  O Fs [Opt Debug Inspect Src WPO?]
  -> ((foreign scm.shen-scheme-compile-native-forms-direct) O Fs Opt Debug Inspect Src WPO?))

(define native-compile-forms/emit
  Scm O Fs [Opt Debug Inspect Src WPO?]
  -> ((foreign scm.shen-scheme-compile-native-forms) Scm O Fs Opt Debug Inspect Src WPO?))

(define native-compile-app
  Root Ms Fs O [Opt Debug Inspect Src _] WPO?
  -> ((foreign scm.shen-scheme-compile-native-app) Root Ms Fs O Opt Debug Inspect Src WPO?))

(define build-app
  M Ms O -> (build-app/emit/options M Ms O "_build/native-app" (native-compile-options) false))

(define build-app/wpo
  M Ms O -> (build-app/emit/options M Ms O "_build/native-app" (native-compile-options) true))

(define build-app/profile
  M Ms O P -> (build-app/emit/options M Ms O "_build/native-app" (native-compile-profile-options P) false))

(define build-app/wpo/profile
  M Ms O P -> (build-app/emit/options M Ms O "_build/native-app" (native-compile-profile-options P) true))

(define build-app/emit/options
  M Ms O Dir Os WPO?
  -> (let Ss (native-app-sources M Ms)
          WPO? (native-effective-wpo? Os WPO?)
          K (native-app-key Ss [O | Os] WPO?)
          AppS (native-app-name-string K)
          App (intern AppS)
          Root (native-app-root Dir K)
          MF (native-app-module-forms Ss App)
          PF (native-app-install-forms (length Ss))
       (native-compile-app Root MF PF O Os WPO?)))

(define build-module-app
  F Dir O -> (build-module-app/emit/options F Dir O "_build/native-module-app" (native-compile-options) false))

(define build-module-app/wpo
  F Dir O -> (build-module-app/emit/options F Dir O "_build/native-module-app" (native-compile-options) true))

(define build-module-app/profile
  F Dir O P -> (build-module-app/emit/options F Dir O "_build/native-module-app" (native-compile-profile-options P) false))

(define build-module-app/wpo/profile
  F Dir O P -> (build-module-app/emit/options F Dir O "_build/native-module-app" (native-compile-profile-options P) true))

(define build-module-app/emit/options
  F Dir O Base Os WPO?
  -> (let D (native-read-module-declaration F)
          Ds (native-module-app-declarations D Dir)
          WPO? (native-effective-wpo? Os WPO?)
          K (native-module-app-key Ds [O | Os] WPO?)
          AppS (native-app-name-string K)
          App (intern AppS)
          Root (native-app-root Base K)
          MF (native-module-app-module-forms Ds App)
          PF (native-app-install-forms (length Ds))
       (native-compile-app Root MF PF O Os WPO?)))

(define compile-module
  F O -> (compile-module/declaration (native-read-module-declaration F) O))

(define compile-module/declaration
  D O -> (do (native-require-module-dir (native-module-declaration-requires D) (fail))
             (compile-module/declaration* D O)))

(define compile-module/declaration*
  D O -> (let Os (native-compile-profile-options (native-module-declaration-profile D))
              Fs (native-module-declaration-scheme-forms D)
           (native-compile-forms/direct O Fs Os)))

(define compile-module/in-dir
  F O Dir -> (compile-module/declaration/in-dir (native-read-module-declaration F) O Dir))

(define compile-module/declaration/in-dir
  D O Dir -> (with-native-compiler-state
              (freeze
               (do (native-prepare-module-requirements
                    (native-module-declaration-requires D) Dir
                    [(native-module-declaration-name D)] [])
                   (compile-module/declaration* D O)))))

(define compile-module/emit
  F O Scm -> (compile-module/emit/declaration (native-read-module-declaration F) O Scm))

(define compile-module/emit/declaration
  D O Scm -> (do (native-require-module-dir (native-module-declaration-requires D) (fail))
                 (compile-module/emit/declaration* D O Scm)))

(define compile-module/emit/declaration*
  D O Scm -> (let Os (native-compile-profile-options (native-module-declaration-profile D))
                  Fs (native-module-declaration-scheme-forms D)
               (native-compile-forms/emit Scm O Fs Os)))

(define compile-module/emit/in-dir
  F O Scm Dir -> (compile-module/emit/declaration/in-dir (native-read-module-declaration F) O Scm Dir))

(define compile-module/emit/declaration/in-dir
  D O Scm Dir -> (with-native-compiler-state
                  (freeze
                   (do (native-prepare-module-requirements
                        (native-module-declaration-requires D) Dir
                        [(native-module-declaration-name D)] [])
                       (compile-module/emit/declaration* D O Scm)))))

(define compile-file
  S O -> (compile-file/options S O (native-compile-options)))

(define compile-file/mode
  S O M -> (compile-file/options/mode S O (native-compile-options) M))

(define compile-file/profile
  S O P -> (compile-file/options S O (native-compile-profile-options P)))

(define compile-file/profile/mode
  S O P M -> (compile-file/options/mode S O (native-compile-profile-options P) M))

(define compile-file/options
  S O Os -> (compile-file/options/mode S O Os compatible))

(define compile-file/options/mode
  S O Os M -> (native-compile-forms/direct O (native-scheme-forms/mode S M) Os))

(define compile-file/emit
  S O Scm -> (compile-file/emit/options S O Scm (native-compile-options)))

(define compile-file/emit/mode
  S O Scm M -> (compile-file/emit/options/mode S O Scm (native-compile-options) M))

(define compile-file/emit/profile
  S O Scm P -> (compile-file/emit/options S O Scm (native-compile-profile-options P)))

(define compile-file/emit/profile/mode
  S O Scm P M -> (compile-file/emit/options/mode S O Scm (native-compile-profile-options P) M))

(define compile-file/emit/options
  S O Scm Os -> (compile-file/emit/options/mode S O Scm Os compatible))

(define compile-file/emit/options/mode
  S O Scm Os M -> (native-compile-forms/emit Scm O (native-scheme-forms/mode S M) Os))

(define delete-file-if-exists
  F -> ((foreign scm.shen-scheme-delete-file-if-exists) F))

(define shen-scheme.file-exists?
  F -> ((foreign scm.file-exists?) F))

)
