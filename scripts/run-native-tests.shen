\* Copyright (c) 2012-2021 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(define native-test.assert-equal
  Label Expected Expected -> (pr (make-string "[OK]    ~A~%" Label))
  Label Expected Actual -> (error "native test failed: ~A expected ~R, got ~R~%"
                                  Label Expected Actual))

(define native-test.contains-form?
  X X -> true
  X [Y | Ys] -> (if (native-test.contains-form? X Y)
                    true
                    (native-test.contains-form? X Ys))
  _ _ -> false)

(define native-test.unit-compiletime
  [native-unit _ _ CT _] -> CT)

(define native-test.compiletime-kind
  [_ [quote [defmacro | _]]] -> defmacro
  [_ [quote [synonyms | _]]] -> synonyms
  [_ _ _] -> declare)

(define native-test.assert-facade-arities
  [] -> true
  [[Function Expected] | Rest]
  -> (do
      (native-test.assert-equal
       (make-string "native facade arity ~A" Function)
       Expected
       (arity Function))
      (native-test.assert-facade-arities Rest)))

(define native-test.assert-unprefixed-symbols
  _ _ [] -> true
  Label Forms [Name | Rest]
  -> (let Expected (intern Name)
          Prefixed (intern (@s "shen-scheme." Name))
       (do
         (native-test.assert-equal
          (make-string "~A contains ~A" Label Expected)
          true
          (native-test.contains-form? Expected Forms))
         (native-test.assert-equal
          (make-string "~A omits package-prefixed ~A" Label Expected)
          false
          (native-test.contains-form? Prefixed Forms))
         (native-test.assert-unprefixed-symbols Label Forms Rest))))

(define native-test.write-file
  File Body -> (let Out (open File out)
                 (do (pr Body Out)
                     (close Out))))

(define native-test.basename
  (@s "_build/native-tests/" File) -> File)

(define native-test.delete-file-if-exists
  File -> (shen-scheme.delete-file-if-exists File))

(define native-test.module-lib-source
  -> "(define native-module-helper
  X -> (+ X 37))
")

(define native-test.module-main-source
  -> "(define native-module-private
  X -> (+ X 1000))

(define native-module-main
  X -> (native-module-helper X))
")

(define native-test.module-default-source
  -> "(define native-module-default-main
  X -> (+ X 1))
")

(define native-test.module-typed-source
  -> "(define native-module-typed
  { number --> number }
  X -> (+ X 1))
")

(define native-test.module-runtime-only-source
  -> "(define native-module-runtime-only
  { number --> number }
  X -> (+ X 1))
")

(define native-test.module-declared-source
  -> "(declare native-module-declared [number --> number])

(define native-module-declared
  X -> (+ X 1))
")

(define native-test.module-runtime-only-declared-source
  -> "(declare native-module-runtime-only-declared [number --> number])

(define native-module-runtime-only-declared
  X -> (+ X 1))
")

(define native-test.module-macro-source
  -> "(defmacro native-module-twice-macro
  [native-module-twice X] -> [* 2 X])
")

(define native-test.module-macro-user-source
  -> "(define native-module-macro-user
  X -> (native-module-twice X))
")

(define native-test.module-synonym-source
  -> "(synonyms native-count number)
")

(define native-test.module-synonym-user-source
  -> "(define native-module-synonym-user
  { native-count --> native-count }
  X -> (+ X 1))
")

(define native-test.module-datatype-source
  -> "(datatype native-small
  if (element? X [0 1])
  ________________
  X : native-small;)
")

(define native-test.module-datatype-user-source
  -> "(define native-module-datatype-user
  { number --> native-small }
  _ -> 1)
")

(define native-test.prolog-source
  -> "(defprolog native-prolog-nreverse
  [] [] <--;
  [X | Y] R <-- (native-prolog-nreverse Y RY) (native-prolog-nappend RY [X] R);)

(defprolog native-prolog-nappend
  [] X X <--;
  [X | Y] Z [X | W] <-- (native-prolog-nappend Y Z W);)
")

(define native-test.module-source-kl-source
  -> "(define native-module-source-kl
  X -> (+ X 1))
")

(define native-test.module-no-source-kl-source
  -> "(define native-module-no-source-kl
  X -> (+ X 1))
")

(define native-test.module-required-source
  -> "(define native-module-required-helper
  X -> (+ X 10))
")

(define native-test.module-requirer-source
  -> "(define native-module-requirer
  X -> (native-module-required-helper X))
")

(define native-test.module-private-arity-source
  -> "(define shen-scheme.native-compile-profile-options
  X Y -> (= X Y))

(define native-module-private-arity-export
  X -> X)
")

(define native-test.module-private-arity-main-source
  -> "(define native-module-private-arity-main
  X -> (= (shen-scheme.native-compile-profile-options X)
          [2 0 false false false]))
")

(define native-test.module-package-source
  -> "(package native.test.dependency.pkg
 [native-module-package-export]

(define native-module-package-export
  X -> (+ X 1))
)
")

(define native-test.module-package-main-source
  -> "(package native.test.package-main
 (append [native-module-package-main] (external native.test.dependency.pkg))

(define native-module-package-main
  X -> (native-module-package-export X))
)
")

(define native-test.module-app-base-source
  -> "(set *native-module-app-init-events* [])

(define native-module-app-private
  X -> (+ X 1000))

(define native-module-app-base
  X -> (+ X 10))

(set *native-module-app-init-events*
     (append (value *native-module-app-init-events*)
             [(native-module-app-base 0)]))
")

(define native-test.module-app-base-updated-source
  -> "(define native-module-app-base
  X -> (+ X 1000))
")

(define native-test.module-app-main-source
  -> "(set *native-module-app-init-events*
     (append (value *native-module-app-init-events*)
             [(native-module-app-main 1)]))

(define native-module-app-main
  X -> (native-module-app-base X))
")

(define native-test.module-app-private-call-source
  -> "(define native-module-app-private-probe
  X -> (native-module-app-private X))
")

(define native-test.module-declaration-source
  Lib Main -> (make-string "(shen.module
  (version 1)
  (name native.test.module)
  (requires-features shen/scheme)
  (sources tc- ~S ~S)
  (extension example/tool
    (flag true))
  (extension shen/scheme
    (mode sealed)
    (exports native-module-main)
    (metadata compiletime runtime)
    (profile debug)))
"
                           (native-test.basename Lib)
                           (native-test.basename Main)))

(define native-test.module-default-declaration-source
  Source -> (make-string "(shen.module
  (version 1)
  (name native.test.defaults)
  (sources tc- ~S)
  (extension shen/scheme))
"
                         (native-test.basename Source)))

(define native-test.module-typed-declaration-source
  Source -> (make-string "(shen.module
  (version 1)
  (name native.test.typed)
  (sources tc+ ~S)
  (extension shen/scheme
    (mode sealed)
    (exports native-module-typed)))
"
                         (native-test.basename Source)))

(define native-test.module-runtime-only-declaration-source
  Source -> (make-string "(shen.module
  (version 1)
  (name native.test.runtime-only)
  (sources tc- ~S)
  (extension shen/scheme
    (mode sealed)
    (exports native-module-runtime-only)
    (metadata runtime)))
"
                         (native-test.basename Source)))

(define native-test.module-declared-declaration-source
  Source -> (make-string "(shen.module
  (version 1)
  (name native.test.declared)
  (sources tc- ~S)
  (extension shen/scheme
    (mode sealed)
    (exports native-module-declared)
    (metadata runtime compiletime)))
"
                         (native-test.basename Source)))

(define native-test.module-runtime-only-declared-declaration-source
  Source -> (make-string "(shen.module
  (version 1)
  (name native.test.runtime-only-declared)
  (sources tc- ~S)
  (extension shen/scheme
    (mode sealed)
    (exports native-module-runtime-only-declared)
    (metadata runtime)))
"
                         (native-test.basename Source)))

(define native-test.module-compiletime-only-declaration-source
  Name Source -> (make-string "(shen.module
  (version 1)
  (name ~A)
  (sources tc- ~S)
  (extension shen/scheme
    (mode sealed)
    (metadata compiletime)))
"
                              Name
                              (native-test.basename Source)))

(define native-test.module-source-kl-declaration-source
  Source -> (make-string "(shen.module
  (version 1)
  (name native.test.source-kl)
  (sources tc- ~S)
  (extension shen/scheme
    (mode sealed)
    (exports native-module-source-kl)
    (metadata runtime source-kl)))
"
                         (native-test.basename Source)))

(define native-test.module-no-source-kl-declaration-source
  Source -> (make-string "(shen.module
  (version 1)
  (name native.test.no-source-kl)
  (sources tc- ~S)
  (extension shen/scheme
    (mode sealed)
    (exports native-module-no-source-kl)
    (metadata runtime compiletime)))
"
                         (native-test.basename Source)))

(define native-test.module-required-declaration-source
  Source -> (make-string "(shen.module
  (version 1)
  (name native.test.required)
  (sources tc- ~S)
  (extension shen/scheme
    (mode sealed)
    (exports native-module-required-helper)))
"
                         (native-test.basename Source)))

(define native-test.module-requirer-declaration-source
  Source -> (make-string "(shen.module
  (version 1)
  (name native.test.requirer)
  (requires native.test.required)
  (sources tc- ~S)
  (extension shen/scheme
    (mode sealed)
    (exports native-module-requirer)))
"
                         (native-test.basename Source)))

(define native-test.module-mismatched-declaration-source
  Source -> (make-string "(shen.module
  (version 1)
  (name native.test.other)
  (sources tc- ~S))
"
                         (native-test.basename Source)))

(define native-test.module-mismatch-requirer-declaration-source
  Source -> (make-string "(shen.module
  (version 1)
  (name native.test.mismatch-requirer)
  (requires native.test.mismatch)
  (sources tc- ~S))
"
                         (native-test.basename Source)))

(define native-test.module-private-arity-declaration-source
  Source -> (make-string "(shen.module
  (version 1)
  (name native.test.private-arity)
  (sources tc- ~S)
  (extension shen/scheme
    (mode sealed)
    (exports native-module-private-arity-export)))
"
                         (native-test.basename Source)))

(define native-test.module-private-arity-main-declaration-source
  Source -> (make-string "(shen.module
  (version 1)
  (name native.test.private-arity-main)
  (requires native.test.private-arity)
  (sources tc- ~S)
  (extension shen/scheme
    (mode sealed)
    (exports native-module-private-arity-main)))
"
                         (native-test.basename Source)))

(define native-test.module-package-declaration-source
  Source -> (make-string "(shen.module
  (version 1)
  (name native.test.package)
  (sources tc- ~S)
  (extension shen/scheme
    (mode sealed)
    (exports native-module-package-export)))
"
                         (native-test.basename Source)))

(define native-test.module-package-main-declaration-source
  Source -> (make-string "(shen.module
  (version 1)
  (name native.test.package-main)
  (requires native.test.package)
  (sources tc- ~S)
  (extension shen/scheme
    (mode sealed)
    (exports native-module-package-main)))
"
                         (native-test.basename Source)))

(define native-test.module-cycle-a-declaration-source
  Source -> (make-string "(shen.module
  (version 1)
  (name native.test.cycle-a)
  (requires native.test.cycle-b)
  (sources tc- ~S)
  (extension shen/scheme
    (mode sealed)))
"
                         (native-test.basename Source)))

(define native-test.module-cycle-b-declaration-source
  Source -> (make-string "(shen.module
  (version 1)
  (name native.test.cycle-b)
  (requires native.test.cycle-a)
  (sources tc- ~S)
  (extension shen/scheme
    (mode sealed)))
"
                         (native-test.basename Source)))

(define native-test.module-app-base-declaration-source
  Source -> (make-string "(shen.module
  (version 1)
  (name native.test.app-base)
  (sources tc- ~S)
  (extension shen/scheme
    (mode sealed)
    (exports native-module-app-base)))
"
                         (native-test.basename Source)))

(define native-test.module-app-main-declaration-source
  Source -> (make-string "(shen.module
  (version 1)
  (name native.test.app-main)
  (requires native.test.app-base)
  (sources tc- ~S)
  (extension shen/scheme
    (mode sealed)
    (exports native-module-app-main)))
"
                         (native-test.basename Source)))

(define native-test.module-app-private-call-declaration-source
  Source -> (make-string "(shen.module
  (version 1)
  (name native.test.app-private-call)
  (requires native.test.app-base)
  (sources tc- ~S)
  (extension shen/scheme
    (mode sealed)
    (exports native-module-app-private-probe)))
"
                         (native-test.basename Source)))

(define native-test.module-app-missing-require-declaration-source
  Source -> (make-string "(shen.module
  (version 1)
  (name native.test.app-missing-require)
  (requires native.test.app-missing)
  (sources tc- ~S)
  (extension shen/scheme
    (mode sealed)
    (exports native-module-app-main)))
"
                         (native-test.basename Source)))

(define native-test.module-app-bad-export-declaration-source
  Source -> (make-string "(shen.module
  (version 1)
  (name native.test.app-bad-export)
  (sources tc- ~S)
  (extension shen/scheme
    (mode sealed)
    (exports native-module-app-missing)))
"
                         (native-test.basename Source)))

(define native-test.module-bad-declaration-source
  Source -> (make-string "(shen.module
  (version 1)
  (name native.test.bad)
  (sources tc- ~S)
  (unknown-field true))
"
                         (native-test.basename Source)))

(define native-test.module-invalid-typed-source
  -> "(define native-module-tc-invalid
  { number --> number }
  X -> true)
")

(define native-test.module-tc-a-source
  -> "(define native-module-tc-a
  { number --> number }
  X -> (+ X 1))
")

(define native-test.module-tc-b-source
  -> "(define native-module-tc-b
  { number --> number }
  X -> (+ X 10))
")

(define native-test.module-tc-c-source
  -> "(define native-module-tc-c
  { number --> number }
  X -> (+ X 100))
")

(define native-test.module-tc-ascribed-source
  -> "(define native-module-tc-ascribed
  { number --> number }
  X -> (+ X 1))

(native-module-tc-ascribed 41) : number
")

(define native-test.module-tc-order-declare-source
  -> "(declare native-module-tc-order [string --> string])
")

(define native-test.module-tc-order-define-source
  -> "(define native-module-tc-order
  { number --> number }
  X -> (+ X 1))
")

(define native-test.module-tc-before-synonym-source
  -> "(define native-module-tc-before-synonym
  { native-module-tc-number --> native-module-tc-number }
  X -> (+ X 1))
")

(define native-test.module-tc-late-synonym-source
  -> "(synonyms native-module-tc-number number)
")

(define native-test.module-tc-unchecked-ascription-source
  -> "(set *native-module-tc-unchecked-ascription* 1)
  : (set *native-module-tc-unchecked-ascription* 2)

(define native-module-tc-unchecked-ascription
  -> (value *native-module-tc-unchecked-ascription*))
")

(define native-test.module-single-source-tc-declaration-source
  Name Mode Export Source -> (make-string "(shen.module
  (version 1)
  (name ~A)
  (sources ~A ~S)
  (extension shen/scheme
    (mode sealed)
    (exports ~A)))
"
                                           Name Mode
                                           (native-test.basename Source)
                                           Export))

(define native-test.module-tc-transition-declaration-source
  A B C -> (make-string "(shen.module
  (version 1)
  (name native.test.tc-transition)
  (sources tc+ ~S tc- ~S tc+ ~S)
  (extension shen/scheme
    (mode sealed)
    (exports native-module-tc-a
             native-module-tc-b
             native-module-tc-c)))
"
                         (native-test.basename A)
                         (native-test.basename B)
                         (native-test.basename C)))

(define native-test.module-tc-order-declaration-source
  Declare Define -> (make-string "(shen.module
  (version 1)
  (name native.test.tc-order)
  (sources tc- ~S tc+ ~S)
  (extension shen/scheme
    (mode sealed)
    (exports native-module-tc-order)))
"
                                 (native-test.basename Declare)
                                 (native-test.basename Define)))

(define native-test.module-tc-late-synonym-declaration-source
  Define Synonym -> (make-string "(shen.module
  (version 1)
  (name native.test.tc-late-synonym)
  (sources tc+ ~S ~S)
  (extension shen/scheme
    (mode sealed)
    (exports native-module-tc-before-synonym)))
"
                                  (native-test.basename Define)
                                  (native-test.basename Synonym)))

(define native-test.compile-load
  Source Object Scheme -> (do (shen-scheme.compile-file/emit Source Object Scheme)
                              (shen-scheme.load-compiled Object)))

(define native-test.compile-load/mode
  Source Object Scheme Mode -> (do (shen-scheme.compile-file/emit/mode Source Object Scheme Mode)
                                   (shen-scheme.load-compiled Object)))

(define native-test.compile-load/direct
  Source Object -> (do (shen-scheme.compile-file Source Object)
                       (shen-scheme.load-compiled Object)))

(define native-test.compile-load/direct/mode
  Source Object Mode -> (do (shen-scheme.compile-file/mode Source Object Mode)
                            (shen-scheme.load-compiled Object)))

(define native-test.compile-load/direct/profile
  Source Object Profile -> (do (shen-scheme.compile-file/profile Source Object Profile)
                               (shen-scheme.load-compiled Object)))

(define native-test.run-refactor-regressions
  -> (let Source "tests/native/simple.shen"
          CompatibleForms (shen-scheme.native-scheme-forms/mode
                           Source
                           compatible)
          SealedForms (shen-scheme.native-scheme-forms/mode
                       Source
                       sealed)
          AppName native-test-refactor-app
          AppForms (append
                    (shen-scheme.native-app-module-forms
                     ["tests/native/app-lib.shen"
                      "tests/native/app-main.shen"]
                     AppName)
                    (shen-scheme.native-app-install-forms 2))
          DependencyMap (shen-scheme.native-module-app-required-visible-map
                         [native.test.first native.test.second]
                         [[module-app-map native.test.second 1
                           [[native-test-shared native-test-second]]
                           [[native-test-shared 2]]]
                          [module-app-map native.test.first 0
                           [[native-test-shared native-test-first]]
                           [[native-test-shared 1]]]])
          BadDependency
          (shen-scheme.native-parse-module-declaration
           [shen.module
            [version 1]
            [name native.test.bad-dependency]
            [sources tc- Source]
            [extension shen/scheme
             [mode compatible]
             [exports native-test-add]]])
       (do
         (native-test.assert-facade-arities
          [[shen-scheme.native-compile-profile-options 1]
           [shen-scheme.native-compile-options 0]
           [shen-scheme.compile-file 2]
           [shen-scheme.compile-file/mode 3]
           [shen-scheme.compile-file/profile 3]
           [shen-scheme.compile-file/profile/mode 4]
           [shen-scheme.compile-file/options 3]
           [shen-scheme.compile-file/options/mode 4]
           [shen-scheme.compile-file/emit 3]
           [shen-scheme.compile-file/emit/mode 4]
           [shen-scheme.compile-file/emit/profile 4]
           [shen-scheme.compile-file/emit/profile/mode 5]
           [shen-scheme.compile-file/emit/options 4]
           [shen-scheme.compile-file/emit/options/mode 5]
           [shen-scheme.compile-module 2]
           [shen-scheme.compile-module/in-dir 3]
           [shen-scheme.compile-module/emit 3]
           [shen-scheme.compile-module/emit/in-dir 4]
           [shen-scheme.load-module 3]
           [shen-scheme.build-app 3]
           [shen-scheme.build-app/wpo 3]
           [shen-scheme.build-app/profile 4]
           [shen-scheme.build-app/wpo/profile 4]
           [shen-scheme.build-app/emit/options 6]
           [shen-scheme.build-module-app 3]
           [shen-scheme.build-module-app/wpo 3]
           [shen-scheme.build-module-app/profile 4]
           [shen-scheme.build-module-app/wpo/profile 4]
           [shen-scheme.build-module-app/emit/options 6]
           [shen-scheme.load-compiled 1]
           [shen-scheme.delete-file-if-exists 1]
           [shen-scheme.file-exists? 1]])
         (native-test.assert-unprefixed-symbols
          "compatible generated forms"
          CompatibleForms
          ["define"
           "cond"
           "quote"
           "true"
           "kl:update-lambda-table"])
         (native-test.assert-unprefixed-symbols
          "sealed generated forms"
          SealedForms
          ["module"
           "import"
           "define"
           "cond"
           "quote"
           "true"
           "define-top-level-value"
           "kl:update-lambda-table"])
         (native-test.assert-unprefixed-symbols
          "app generated forms"
          AppForms
          ["module"
           "define"
           "quote"
           "define-top-level-value"
           "kl:update-lambda-table"])
         (native-test.assert-equal
          "module app later dependency precedence"
          native-test-second
          (_scm.with-native-context
           app
           DependencyMap
           (freeze (_scm.native-local-name native-test-shared))))
         (native-test.assert-equal
          "compatible dependency explicit exports rejected"
          failed
          (trap-error
           (shen-scheme.native-prepare-module/declaration*
            BadDependency "" [] [])
           (/. E failed)))
         (native-test.assert-equal
          "compatible direct name skips source key"
          skip
          (shen-scheme.native-module-name/mode
           "_build/native-tests/missing-key-source.shen"
           compatible))
         (native-test.assert-equal
          "compatible declaration name skips source key"
          skip
         (shen-scheme.native-module-declaration-module-name/mode
           (shen-scheme.native-parse-module-declaration
            [shen.module
             [version 1]
             [name native.test.missing-key]
             [sources tc-
              "_build/native-tests/missing-key-source.shen"]])
           compatible)))))

(define native-test.run-private-dependency-arity
  -> (let Dir "_build/native-tests"
          DepSource "_build/native-tests/module-private-arity.shen"
          MainSource "_build/native-tests/module-private-arity-main.shen"
          DepDeclaration "_build/native-tests/native.test.private-arity.shenmod"
          MainDeclaration "_build/native-tests/native.test.private-arity-main.shenmod"
          Object "_build/native-tests/module-private-arity-main.so"
          AppObject "_build/native-tests/module-private-arity-app.so"
          Assert (/. L E A (native-test.assert-equal L E A))
       (do
         (native-test.write-file
          DepSource
          (native-test.module-private-arity-source))
         (native-test.write-file
          MainSource
          (native-test.module-private-arity-main-source))
         (native-test.write-file
          DepDeclaration
          (native-test.module-private-arity-declaration-source DepSource))
         (native-test.write-file
          MainDeclaration
          (native-test.module-private-arity-main-declaration-source MainSource))
         (shen-scheme.compile-module/in-dir MainDeclaration Object Dir)
         (shen-scheme.load-compiled Object)
         (Assert "module compile ignores private dependency arity"
                 true
                 (eval [native-module-private-arity-main release]))
         (let Result (shen-scheme.build-module-app MainDeclaration Dir AppObject)
           (do
             (shen-scheme.load-compiled (hd Result))
             (Assert "module app ignores private dependency arity"
                     true
                     (eval [native-module-private-arity-main release])))))))

(define native-test.run-dependency-package-metadata
  -> (let Dir "_build/native-tests"
          DepSource "_build/native-tests/module-package.shen"
          MainSource "_build/native-tests/module-package-main.shen"
          DepDeclaration "_build/native-tests/native.test.package.shenmod"
          MainDeclaration "_build/native-tests/native.test.package-main.shenmod"
          Object "_build/native-tests/module-package-main.so"
          AppObject "_build/native-tests/module-package-app.so"
          Assert (/. L E A (native-test.assert-equal L E A))
       (do
         (native-test.write-file DepSource (native-test.module-package-source))
         (native-test.write-file MainSource (native-test.module-package-main-source))
         (native-test.write-file
          DepDeclaration
          (native-test.module-package-declaration-source DepSource))
         (native-test.write-file
          MainDeclaration
          (native-test.module-package-main-declaration-source MainSource))
         (shen-scheme.compile-module/in-dir MainDeclaration Object Dir)
         (Assert "module compile preserves dependency package metadata"
                 true
                 (shen-scheme.file-exists? Object))
         (let Result (shen-scheme.build-module-app MainDeclaration Dir AppObject)
           (do
             (shen-scheme.load-compiled (hd Result))
             (Assert "module app preserves dependency package metadata"
                     42
                     (eval [native-module-package-main 41])))))))

(define native-test.assert-simple
  Prefix Add Inc Sumdown MapInc -> (let Assert (/. Label Expected Actual
                                                 (native-test.assert-equal
                                                   (@s Prefix Label)
                                                   Expected
                                                   Actual))
                                     (do
                                       (Assert " add" Add (eval [native-test-add 19 23]))
                                       (Assert " inc" Inc (eval [native-test-inc 7]))
                                       (Assert " recursion" Sumdown (eval [native-test-sumdown 5]))
                                       (Assert " list" MapInc (eval [native-test-map-inc [cons 1 [cons 2 [cons 3 []]]]])))))

(define native-test.run-compatible-redefinition
  -> (do
    (native-test.compile-load
      "tests/native/redefinition-compatible.shen"
      "_build/native-tests/redefinition-compatible.so"
      "_build/native-tests/redefinition-compatible.scm")
    (native-test.assert-equal "compatible before redefinition" 6 (eval [native-compatible-main 5]))
    (load "tests/native/redefinition-compatible-updated.shen")
    (native-test.assert-equal "compatible helper redefined" 105 (eval [native-compatible-helper 5]))
    (native-test.assert-equal "compatible caller observes redefinition" 105 (eval [native-compatible-main 5]))))

(define native-test.run-sealed-redefinition
  -> (do
    (native-test.compile-load/mode
      "tests/native/redefinition-sealed.shen"
      "_build/native-tests/redefinition-sealed.so"
      "_build/native-tests/redefinition-sealed.scm"
      sealed)
    (native-test.assert-equal "sealed before redefinition" 6 (eval [native-sealed-main 5]))
    (load "tests/native/redefinition-sealed-updated.shen")
    (native-test.assert-equal "sealed helper redefined" 105 (eval [native-sealed-helper 5]))
    (native-test.assert-equal "sealed caller keeps local helper" 6 (eval [native-sealed-main 5]))))

(define native-test.run-direct-compile
  -> (let DirectObject "_build/native-tests/api-direct.so"
          DirectScheme (shen-scheme.native-scheme-path DirectObject)
          SealedObject "_build/native-tests/api-direct-sealed.so"
          SealedScheme (shen-scheme.native-scheme-path SealedObject)
          Assert (/. Label Expected Actual
                    (native-test.assert-equal Label Expected Actual))
       (do
         (native-test.delete-file-if-exists DirectScheme)
         (native-test.delete-file-if-exists SealedScheme)
         (native-test.compile-load/direct "tests/native/simple.shen" DirectObject)
         (native-test.assert-simple "api direct load" 42 8 15 [2 3 4])
         (Assert "api direct does not emit scheme" false
                 (shen-scheme.file-exists? DirectScheme))
         (native-test.compile-load/direct/mode "tests/native/redefinition-sealed.shen" SealedObject sealed)
         (Assert "api direct sealed compile" 6 (eval [native-sealed-main 5]))
         (Assert "api direct sealed does not emit scheme" false
                 (shen-scheme.file-exists? SealedScheme)))))

(define native-test.run-prolog
  -> (let Source "_build/native-tests/prolog.shen"
          Object "_build/native-tests/prolog-sealed.so"
          Query "(prolog? (native-prolog-nreverse [1 2 3 4] X) (return X))"
          Assert (/. Label Expected Actual
                    (native-test.assert-equal Label Expected Actual))
       (do
         (native-test.write-file Source (native-test.prolog-source))
         (native-test.compile-load/direct/mode Source Object sealed)
         (Assert "sealed defprolog query"
                 [4 3 2 1]
                 (eval (hd (read-from-string Query)))))))

(define native-test.run-package-effects
  -> (let Source "tests/native/package-effects.shen"
          Updated "tests/native/package-effects-updated.shen"
          Compatible "_build/native-tests/package-effects-compatible.so"
          Sealed "_build/native-tests/package-effects-sealed.so"
          Assert (/. L E A (native-test.assert-equal L E A))
       (do
         (set *native-package-events* [compile])
         (Assert "package compile-time metadata order"
                 [defmacro synonyms declare]
                 (map (function native-test.compiletime-kind)
                      (native-test.unit-compiletime
                       (shen-scheme.native-source->unit Source))))
         (shen-scheme.compile-file Source Compatible)
         (Assert "compatible effects deferred" [compile]
                 (value *native-package-events*))
         (shen-scheme.load-compiled Compatible)
         (Assert "compatible package call" 12 (eval [native-package-main 5]))
         (Assert "compatible package effect order" [41 2]
                 (value *native-package-events*))
         (Assert "compatible package external preserved" true
                 (element? native-package-main (external native.test.pkg)))
         (Assert "compatible package internal qualified" true
                 (element? native.test.pkg.helper (internal native.test.pkg)))
         (Assert "compatible package declaration qualified" true
                 (not (= [] (assoc native.test.pkg.helper
                                   (value shen.*sigf*)))))
         (shen-scheme.compile-file/mode Source Sealed sealed)
         (Assert "sealed effects deferred" [41 2]
                 (value *native-package-events*))
         (shen-scheme.load-compiled Sealed)
         (Assert "sealed package effect order" [41 2]
                 (eval [native-package-state]))
         (load Updated)
         (Assert "sealed qualified helper redefined" 1005
                 (eval [native.test.pkg.helper 5]))
         (Assert "sealed package caller keeps local helper" 12
                 (eval [native-package-main 5])))))

(define native-test.run-profiles
  -> (let Assert (/. Label Expected Actual
                    (native-test.assert-equal Label Expected Actual))
          DebugOptions (shen-scheme.native-compile-profile-options debug)
          WpoOptions (shen-scheme.native-compile-profile-options wpo)
          UnsafeOptions (shen-scheme.native-compile-profile-options unsafe)
          DebugObject "_build/native-tests/api-profile-debug.so"
          DebugScheme (shen-scheme.native-scheme-path DebugObject)
       (do
         (Assert "profile release options" [2 0 false false false]
                 (shen-scheme.native-compile-profile-options release))
         (Assert "profile debug options" [2 2 true true false] DebugOptions)
         (Assert "profile wpo options" [2 0 false false true] WpoOptions)
         (Assert "profile unsafe options" [3 0 false false false] UnsafeOptions)
         (Assert "profile string lookup" [2 2 true true false]
                 (shen-scheme.native-compile-profile-options "debug"))
         (Assert "profile wpo enables app WPO" true
                 (shen-scheme.native-effective-wpo? WpoOptions false))
         (Assert "explicit WPO composes with unsafe profile" true
                 (shen-scheme.native-effective-wpo? UnsafeOptions true))
         (native-test.delete-file-if-exists DebugScheme)
         (native-test.compile-load/direct/profile
          "tests/native/simple.shen"
          DebugObject
          debug)
         (native-test.assert-simple "api debug profile load" 42 8 15 [2 3 4])
         (Assert "api debug profile does not emit scheme"
                 false
                 (shen-scheme.file-exists? DebugScheme)))))

(define native-test.run-module-declarations
  -> (let ModuleDir "_build/native-tests"
          ObjectDir "_build/native-tests/module-objects"
          Lib "_build/native-tests/module-lib.shen"
          Main "_build/native-tests/module-main.shen"
          DefaultSource "_build/native-tests/module-default.shen"
          TypedSource "_build/native-tests/module-typed.shen"
          RuntimeOnlySource "_build/native-tests/module-runtime-only.shen"
          DeclaredSource "_build/native-tests/module-declared.shen"
          RuntimeOnlyDeclaredSource "_build/native-tests/module-runtime-only-declared.shen"
          MacroSource "_build/native-tests/module-macro.shen"
          MacroUserSource "_build/native-tests/module-macro-user.shen"
          SynonymSource "_build/native-tests/module-synonym.shen"
          SynonymUserSource "_build/native-tests/module-synonym-user.shen"
          DatatypeSource "_build/native-tests/module-datatype.shen"
          DatatypeUserSource "_build/native-tests/module-datatype-user.shen"
          SourceKlSource "_build/native-tests/module-source-kl.shen"
          NoSourceKlSource "_build/native-tests/module-no-source-kl.shen"
          RequiredSource "_build/native-tests/module-required.shen"
          RequirerSource "_build/native-tests/module-requirer.shen"
          Declaration "_build/native-tests/module-decl.shenmod"
          DefaultDeclaration "_build/native-tests/module-default.shenmod"
          TypedDeclaration "_build/native-tests/module-typed.shenmod"
          RuntimeOnlyDeclaration "_build/native-tests/module-runtime-only.shenmod"
          DeclaredDeclaration "_build/native-tests/module-declared.shenmod"
          RuntimeOnlyDeclaredDeclaration "_build/native-tests/module-runtime-only-declared.shenmod"
          MacroDeclaration "_build/native-tests/module-macro.shenmod"
          SynonymDeclaration "_build/native-tests/module-synonym.shenmod"
          DatatypeDeclaration "_build/native-tests/module-datatype.shenmod"
          SourceKlDeclaration "_build/native-tests/module-source-kl.shenmod"
          NoSourceKlDeclaration "_build/native-tests/module-no-source-kl.shenmod"
          RequiredDeclaration "_build/native-tests/native.test.required.shenmod"
          RequirerDeclaration "_build/native-tests/native.test.requirer.shenmod"
          CycleADeclaration "_build/native-tests/native.test.cycle-a.shenmod"
          CycleBDeclaration "_build/native-tests/native.test.cycle-b.shenmod"
          MismatchDeclaration
          "_build/native-tests/native.test.mismatch.shenmod"
          MismatchRequirerDeclaration
          "_build/native-tests/module-mismatch-requirer.shenmod"
          MismatchError
          (make-string
           "native module required native.test.mismatch but ~A declares native.test.other~%"
           MismatchDeclaration)
          BadDeclaration "_build/native-tests/module-bad.shenmod"
          Object "_build/native-tests/module-decl.so"
          DefaultObject "_build/native-tests/module-default.so"
          TypedObject "_build/native-tests/module-typed.so"
          RuntimeOnlyObject "_build/native-tests/module-runtime-only.so"
          RequiredObject
          "_build/native-tests/module-objects/native.test.required.so"
          RequirerObject
          "_build/native-tests/module-objects/native.test.requirer.so"
          RequirerEmitObject
          "_build/native-tests/module-objects/native.test.requirer.emit.so"
          RequirerScheme "_build/native-tests/native.test.requirer.emit.scm"
          Assert (/. Label Expected Actual
                    (native-test.assert-equal Label Expected Actual))
       (do
         (native-test.write-file Lib (native-test.module-lib-source))
         (native-test.write-file Main (native-test.module-main-source))
         (native-test.write-file DefaultSource (native-test.module-default-source))
         (native-test.write-file TypedSource (native-test.module-typed-source))
         (native-test.write-file RuntimeOnlySource (native-test.module-runtime-only-source))
         (native-test.write-file DeclaredSource (native-test.module-declared-source))
         (native-test.write-file RuntimeOnlyDeclaredSource
                                 (native-test.module-runtime-only-declared-source))
         (native-test.write-file MacroSource (native-test.module-macro-source))
         (native-test.write-file MacroUserSource (native-test.module-macro-user-source))
         (native-test.write-file SynonymSource (native-test.module-synonym-source))
         (native-test.write-file SynonymUserSource
                                 (native-test.module-synonym-user-source))
         (native-test.write-file DatatypeSource (native-test.module-datatype-source))
         (native-test.write-file DatatypeUserSource
                                 (native-test.module-datatype-user-source))
         (native-test.write-file SourceKlSource
                                 (native-test.module-source-kl-source))
         (native-test.write-file NoSourceKlSource
                                 (native-test.module-no-source-kl-source))
         (native-test.write-file RequiredSource
                                 (native-test.module-required-source))
         (native-test.write-file RequirerSource
                                 (native-test.module-requirer-source))
         (native-test.write-file
          Declaration
          (native-test.module-declaration-source Lib Main))
         (native-test.write-file
          DefaultDeclaration
          (native-test.module-default-declaration-source DefaultSource))
         (native-test.write-file
          TypedDeclaration
          (native-test.module-typed-declaration-source TypedSource))
         (native-test.write-file
          RuntimeOnlyDeclaration
          (native-test.module-runtime-only-declaration-source RuntimeOnlySource))
         (native-test.write-file
          DeclaredDeclaration
          (native-test.module-declared-declaration-source DeclaredSource))
         (native-test.write-file
          RuntimeOnlyDeclaredDeclaration
          (native-test.module-runtime-only-declared-declaration-source
           RuntimeOnlyDeclaredSource))
         (native-test.write-file
          MacroDeclaration
          (native-test.module-compiletime-only-declaration-source
           native.test.macro
           MacroSource))
         (native-test.write-file
          SynonymDeclaration
          (native-test.module-compiletime-only-declaration-source
           native.test.synonym
           SynonymSource))
         (native-test.write-file
          DatatypeDeclaration
          (native-test.module-compiletime-only-declaration-source
           native.test.datatype
           DatatypeSource))
         (native-test.write-file
          SourceKlDeclaration
          (native-test.module-source-kl-declaration-source SourceKlSource))
         (native-test.write-file
          NoSourceKlDeclaration
          (native-test.module-no-source-kl-declaration-source NoSourceKlSource))
         (native-test.write-file
          RequiredDeclaration
          (native-test.module-required-declaration-source RequiredSource))
         (native-test.write-file
          RequirerDeclaration
          (native-test.module-requirer-declaration-source RequirerSource))
         (native-test.write-file
          CycleADeclaration
          (native-test.module-cycle-a-declaration-source DefaultSource))
         (native-test.write-file
          CycleBDeclaration
          (native-test.module-cycle-b-declaration-source DefaultSource))
         (native-test.write-file
          MismatchDeclaration
          (native-test.module-mismatched-declaration-source DefaultSource))
         (native-test.write-file
          MismatchRequirerDeclaration
          (native-test.module-mismatch-requirer-declaration-source DefaultSource))
         (native-test.write-file
          BadDeclaration
          (native-test.module-bad-declaration-source DefaultSource))
         (let Module (shen-scheme.native-read-module-declaration Declaration)
              Defaults (shen-scheme.native-read-module-declaration DefaultDeclaration)
              ExpectedSources
              [(shen-scheme.native-resolve-module-source-path
                Declaration (native-test.basename Lib))
               (shen-scheme.native-resolve-module-source-path
                Declaration (native-test.basename Main))]
           (do
             (Assert "module declaration name"
                     native.test.module
                     (shen-scheme.native-module-declaration-name Module))
             (Assert "module declaration mode"
                     sealed
                     (shen-scheme.native-module-declaration-mode Module))
             (Assert "module declaration profile"
                     debug
                     (shen-scheme.native-module-declaration-profile Module))
             (Assert "module declaration sources"
                     ExpectedSources
                     (shen-scheme.native-module-declaration-sources Module))
             (Assert "module declaration resolved sources exist"
                     true
                     (every? (function shen-scheme.file-exists?)
                             ExpectedSources))
             (Assert "module declaration rejects absolute sources"
                     failed
                     (trap-error
                      (shen-scheme.native-parse-module-declaration
                       [shen.module
                        [version 1]
                        [name absolute-source]
                        [sources tc- (hd ExpectedSources)]])
                      (/. E failed)))
             (Assert "module declaration rejects empty sources"
                     failed
                     (trap-error
                      (shen-scheme.native-parse-module-declaration
                       [shen.module
                        [version 1]
                        [name empty-source]
                        [sources tc- ""]])
                      (/. E failed)))
             (Assert "module declaration source modes"
                     [tc- tc-]
                     (shen-scheme.native-module-declaration-source-modes Module))
             (Assert "module declaration required features"
                     [shen/scheme]
                     (shen-scheme.native-module-declaration-required-features Module))
             (Assert "module declaration preserves foreign extensions"
                     [[flag true]]
                     (shen-scheme.native-module-declaration-extension
                      example/tool Module))
             (Assert "module declaration exports"
                     [native-module-main]
                     (shen-scheme.native-module-declaration-exports Module))
             (Assert "module declaration default mode"
                     compatible
                     (shen-scheme.native-module-declaration-mode Defaults))
             (Assert "module declaration default exports"
                     infer-all
                     (shen-scheme.native-module-declaration-exports Defaults))
             (Assert "module declaration default profile"
                     release
                     (shen-scheme.native-module-declaration-profile Defaults))
             (Assert "module declaration rejects unknown field"
                     failed
                     (trap-error
                      (shen-scheme.native-read-module-declaration BadDeclaration)
                      (/. E failed)))
             (Assert "module declaration rejects legacy head"
                     failed
                     (trap-error
                      (shen-scheme.native-parse-module-declaration
                       [shen.aot.module
                        [name legacy]
                        [sources tc- "legacy.shen"]])
                      (/. E failed)))
             (Assert "module declaration requires version one"
                     failed
                     (trap-error
                      (shen-scheme.native-parse-module-declaration
                       [shen.module
                        [version 2]
                        [name future]
                        [sources tc- "future.shen"]])
                      (/. E failed)))
             (Assert "module declaration requires source mode"
                     failed
                     (trap-error
                      (shen-scheme.native-parse-module-declaration
                       [shen.module
                        [version 1]
                        [name unmarked]
                        [sources "unmarked.shen"]])
                      (/. E failed)))
             (Assert "module declaration rejects unavailable features"
                     failed
                     (trap-error
                      (shen-scheme.native-parse-module-declaration
                       [shen.module
                        [version 1]
                        [name missing-feature]
                        [requires-features native.test.missing-feature]
                        [sources tc- "missing-feature.shen"]])
                      (/. E failed)))
             (Assert "module declaration rejects dangling source mode"
                     failed
                     (trap-error
                      (shen-scheme.native-parse-module-declaration
                       [shen.module
                        [version 1]
                        [name dangling-mode]
                        [sources tc- "one.shen" tc+]])
                      (/. E failed)))
             (Assert "module declaration rejects duplicate extensions"
                     failed
                     (trap-error
                      (shen-scheme.native-parse-module-declaration
                       [shen.module
                        [version 1]
                        [name duplicate-extension]
                        [sources tc- "duplicate-extension.shen"]
                        [extension example/tool]
                        [extension example/tool]])
                      (/. E failed)))
             (Assert "module declaration validates Shen/Scheme extension"
                     failed
                     (trap-error
                      (shen-scheme.native-parse-module-declaration
                       [shen.module
                        [version 1]
                        [name bad-extension]
                        [sources tc- "bad-extension.shen"]
                        [extension shen/scheme [unknown true]]])
                      (/. E failed)))
             (Assert "module metadata accepts arbitrary effects"
                     []
                     (shen-scheme.native-raw-compiletime-forms
                      [[set *native-test-effect* true]]))
             (shen-scheme.compile-module Declaration Object)
             (shen-scheme.load-compiled Object)
             (Assert "module declaration exported call"
                     42
                     (eval [native-module-main 5]))
             (Assert "module declaration private helper hidden"
                     unavailable
                     (trap-error (eval [native-module-helper 5])
                                 (/. E unavailable)))
             (shen-scheme.compile-module DefaultDeclaration DefaultObject)
             (shen-scheme.load-compiled DefaultObject)
             (Assert "module declaration default compile"
                     6
                     (eval [native-module-default-main 5]))
             (shen-scheme.compile-module TypedDeclaration TypedObject)
             (shen-scheme.load-compiled TypedObject)
             (Assert "module declaration typed call"
                     42
                     (eval [native-module-typed 41]))
             (Assert "module declaration compiletime type metadata"
                     true
                     (not (= [] (assoc native-module-typed
                                       (value shen.*sigf*)))))
             (shen-scheme.compile-module RuntimeOnlyDeclaration RuntimeOnlyObject)
             (shen-scheme.load-compiled RuntimeOnlyObject)
             (Assert "module declaration runtime-only typed call"
                     42
                     (eval [native-module-runtime-only 41]))
             (Assert "module declaration runtime-only omits compiletime metadata"
                     []
                     (assoc native-module-runtime-only
                            (value shen.*sigf*)))
             (shen-scheme.compile-module RequiredDeclaration RequiredObject)
             (Assert "module requires needs module dir"
                     failed
                     (trap-error
                      (shen-scheme.compile-module
                       RequirerDeclaration
                       RequirerObject)
                      (/. E failed)))
             (shen-scheme.compile-module/in-dir
              RequirerDeclaration
              RequirerObject
              ModuleDir)
             (shen-scheme.load-module
              RequirerDeclaration
              ModuleDir
              ObjectDir)
             (Assert "module requires loaded dependency"
                     52
                     (eval [native-module-requirer 42]))
             (shen-scheme.compile-module/emit/in-dir
              RequirerDeclaration
              RequirerEmitObject
              RequirerScheme
              ModuleDir)
             (shen-scheme.load-compiled RequirerEmitObject)
             (Assert "module requires emit compile"
                     53
                     (eval [native-module-requirer 43]))
             (Assert "module dependency cycle detected"
                     failed
                     (trap-error
                      (shen-scheme.load-module
                       CycleADeclaration
                       ModuleDir
                       ObjectDir)
                      (/. E failed)))
             (Assert "module compile rejects mismatched required name"
                     MismatchError
                     (trap-error
                      (do (shen-scheme.compile-module/in-dir
                           MismatchRequirerDeclaration
                           RequirerObject
                           ModuleDir)
                          unexpected-success)
                      (/. E (error-to-string E))))
             (Assert "module load rejects mismatched required name"
                     MismatchError
                     (trap-error
                      (do (shen-scheme.load-module
                           MismatchRequirerDeclaration
                           ModuleDir
                           ObjectDir)
                          unexpected-success)
                      (/. E (error-to-string E))))
             (Assert "module app rejects mismatched required name"
                     MismatchError
                     (trap-error
                      (do (shen-scheme.build-module-app
                           MismatchRequirerDeclaration
                           ModuleDir
                           RequirerObject)
                          unexpected-success)
                      (/. E (error-to-string E)))))))))

(define native-test.run-nested-module-graph
  -> (let ModuleDir "_build/native-tests/nested-modules"
          ObjectDir "_build/native-tests/nested-objects"
          NestedSource
          "_build/native-tests/nested-modules/native.test/nested.shen"
          RootSource "_build/native-tests/nested-modules/nested-root.shen"
          NestedDeclaration
          "_build/native-tests/nested-modules/native.test/nested.shenmod"
          RootDeclaration
          "_build/native-tests/nested-modules/native.test.nested-root.shenmod"
          NestedObject
          "_build/native-tests/nested-objects/native.test/nested.so"
          RootObject
          "_build/native-tests/nested-objects/native.test.nested-root.so"
          WrongNestedObject
          "_build/native-tests/nested-modules/native.test/nested.so"
          WrongRootObject
          "_build/native-tests/nested-modules/native.test.nested-root.so"
          AppObject "_build/native-tests/nested-module-app.so"
          Assert (/. Label Expected Actual
                    (native-test.assert-equal Label Expected Actual))
       (do
         (native-test.write-file
          NestedSource
          "(define native-test-nested-helper
  X -> (+ X 10))
")
         (native-test.write-file
          RootSource
          "(define native-test-nested-main
  X -> (native-test-nested-helper X))
")
         (native-test.write-file
          NestedDeclaration
          (make-string "(shen.module
  (version 1)
  (name native.test/nested)
  (sources tc- ~S)
  (extension shen/scheme
    (mode sealed)
    (exports native-test-nested-helper)))
"
                       "nested.shen"))
         (native-test.write-file
          RootDeclaration
          (make-string "(shen.module
  (version 1)
  (name native.test.nested-root)
  (requires native.test/nested)
  (sources tc- ~S)
  (extension shen/scheme
    (mode sealed)
    (exports native-test-nested-main)))
"
                       "nested-root.shen"))
         (native-test.delete-file-if-exists NestedObject)
         (native-test.delete-file-if-exists RootObject)
         (native-test.delete-file-if-exists WrongNestedObject)
         (native-test.delete-file-if-exists WrongRootObject)
         (shen-scheme.compile-module/in-dir
          RootDeclaration RootObject ModuleDir)
         (Assert "nested module compile needs no dependency object"
                 false
                 (shen-scheme.file-exists? NestedObject))
         (let Result
              (shen-scheme.build-module-app RootDeclaration ModuleDir AppObject)
           (do
             (shen-scheme.load-compiled (hd Result))
             (Assert "nested module app traverses slash names"
                     43
                     (eval [native-test-nested-main 33]))))
         (shen-scheme.compile-module NestedDeclaration NestedObject)
         (shen-scheme.load-module RootDeclaration ModuleDir ObjectDir)
         (Assert "nested module graph loads from separate roots"
                 42
                 (eval [native-test-nested-main 32])))))

(define native-test.run-module-source-typechecking
  -> (let InvalidSource "_build/native-tests/module-tc-invalid.shen"
          InvalidDeclaration "_build/native-tests/module-tc-invalid.shenmod"
          UncheckedDeclaration "_build/native-tests/module-tc-unchecked.shenmod"
          InvalidObject "_build/native-tests/module-tc-invalid.so"
          InvalidAppObject "_build/native-tests/module-tc-invalid-app.so"
          UncheckedObject "_build/native-tests/module-tc-unchecked.so"
          A "_build/native-tests/module-tc-a.shen"
          B "_build/native-tests/module-tc-b.shen"
          C "_build/native-tests/module-tc-c.shen"
          TransitionDeclaration "_build/native-tests/module-tc-transition.shenmod"
          TransitionObject "_build/native-tests/module-tc-transition.so"
          AscribedSource "_build/native-tests/module-tc-ascribed.shen"
          AscribedDeclaration "_build/native-tests/module-tc-ascribed.shenmod"
          AscribedObject "_build/native-tests/module-tc-ascribed.so"
          OrderDeclare "_build/native-tests/module-tc-order-declare.shen"
          OrderDefine "_build/native-tests/module-tc-order-define.shen"
          OrderDeclaration "_build/native-tests/module-tc-order.shenmod"
          OrderObject "_build/native-tests/module-tc-order.so"
          BeforeSynonym "_build/native-tests/module-tc-before-synonym.shen"
          LateSynonym "_build/native-tests/module-tc-late-synonym.shen"
          LateSynonymDeclaration
          "_build/native-tests/module-tc-late-synonym.shenmod"
          LateSynonymObject "_build/native-tests/module-tc-late-synonym.so"
          UncheckedAscriptionSource
          "_build/native-tests/module-tc-unchecked-ascription.shen"
          UncheckedAscriptionDeclaration
          "_build/native-tests/module-tc-unchecked-ascription.shenmod"
          UncheckedAscriptionObject
          "_build/native-tests/module-tc-unchecked-ascription.so"
          Assert (/. Label Expected Actual
                    (native-test.assert-equal Label Expected Actual))
       (do
         (native-test.write-file InvalidSource
                                 (native-test.module-invalid-typed-source))
         (native-test.write-file
          InvalidDeclaration
          (native-test.module-single-source-tc-declaration-source
           native.test.tc-invalid tc+ native-module-tc-invalid InvalidSource))
         (native-test.write-file
          UncheckedDeclaration
          (native-test.module-single-source-tc-declaration-source
           native.test.tc-unchecked tc- native-module-tc-invalid InvalidSource))
         (native-test.write-file A (native-test.module-tc-a-source))
         (native-test.write-file B (native-test.module-tc-b-source))
         (native-test.write-file C (native-test.module-tc-c-source))
         (native-test.write-file
          TransitionDeclaration
          (native-test.module-tc-transition-declaration-source A B C))
         (native-test.write-file AscribedSource
                                 (native-test.module-tc-ascribed-source))
         (native-test.write-file
          AscribedDeclaration
          (native-test.module-single-source-tc-declaration-source
           native.test.tc-ascribed tc+ native-module-tc-ascribed
           AscribedSource))
         (native-test.write-file OrderDeclare
                                 (native-test.module-tc-order-declare-source))
         (native-test.write-file OrderDefine
                                 (native-test.module-tc-order-define-source))
         (native-test.write-file
          OrderDeclaration
          (native-test.module-tc-order-declaration-source
           OrderDeclare OrderDefine))
         (native-test.write-file
          BeforeSynonym
          (native-test.module-tc-before-synonym-source))
         (native-test.write-file
          LateSynonym
          (native-test.module-tc-late-synonym-source))
         (native-test.write-file
          LateSynonymDeclaration
          (native-test.module-tc-late-synonym-declaration-source
           BeforeSynonym LateSynonym))
         (native-test.write-file
          UncheckedAscriptionSource
          (native-test.module-tc-unchecked-ascription-source))
         (native-test.write-file
          UncheckedAscriptionDeclaration
          (native-test.module-single-source-tc-declaration-source
           native.test.tc-unchecked-ascription tc-
           native-module-tc-unchecked-ascription
           UncheckedAscriptionSource))
         (Assert "module tc+ rejects invalid signed definition"
                 failed
                 (trap-error
                  (shen-scheme.compile-module InvalidDeclaration InvalidObject)
                  (/. E failed)))
         (Assert "module app tc+ rejects invalid signed definition"
                 failed
                 (trap-error
                  (shen-scheme.build-module-app
                   InvalidDeclaration
                   "_build/native-tests"
                   InvalidAppObject)
                  (/. E failed)))
         (Assert "module tc+ cannot see a later synonym"
                 failed
                 (trap-error
                  (shen-scheme.compile-module
                   LateSynonymDeclaration LateSynonymObject)
                  (/. E failed)))
         (shen-scheme.compile-module UncheckedDeclaration UncheckedObject)
         (shen-scheme.load-compiled UncheckedObject)
         (Assert "module tc- permits invalid signed definition"
                 true
                 (eval [native-module-tc-invalid 0]))
         (Assert "module tc- omits inline signature"
                 []
                 (assoc native-module-tc-invalid (value shen.*sigf*)))
         (shen-scheme.compile-module
          UncheckedAscriptionDeclaration UncheckedAscriptionObject)
         (shen-scheme.load-compiled UncheckedAscriptionObject)
         (Assert "module tc- retains top-level ascription forms"
                 2
                 (eval [native-module-tc-unchecked-ascription]))
         (shen-scheme.compile-module TransitionDeclaration TransitionObject)
         (shen-scheme.load-compiled TransitionObject)
         (Assert "module source mode transitions run"
                 126
                 (eval [+ [native-module-tc-a 5]
                          [+ [native-module-tc-b 5]
                             [native-module-tc-c 5]]]))
         (Assert "module tc+ first source retains inline signature"
                 true
                 (not (= [] (assoc native-module-tc-a
                                   (value shen.*sigf*)))))
         (Assert "module tc- middle source omits inline signature"
                 []
                 (assoc native-module-tc-b (value shen.*sigf*)))
         (Assert "module tc+ final source retains inline signature"
                 true
                 (not (= [] (assoc native-module-tc-c
                                   (value shen.*sigf*)))))
         (shen-scheme.compile-module AscribedDeclaration AscribedObject)
         (shen-scheme.load-compiled AscribedObject)
         (Assert "module tc+ compiles top-level ascription once"
                 42
                 (eval [native-module-tc-ascribed 41]))
         (shen-scheme.compile-module OrderDeclaration OrderObject)
         (shen-scheme.load-compiled OrderObject)
         (Assert "module signatures retain source order"
                 number
                 (shen.typecheck [native-module-tc-order 1]
                                 number)))))

(define native-test.run-app-builder
  -> (let Result (shen-scheme.build-app/profile
                   "tests/native/app-main.shen"
                   ["tests/native/app-lib.shen"]
                   "_build/native-tests/app-wpo.so"
                   wpo)
          Object (hd Result)
          RuntimeLibraries (hd (tl Result))
          Assert (/. Label Expected Actual
                    (native-test.assert-equal Label Expected Actual))
       (do
         (Assert "app builder object" "_build/native-tests/app-wpo.so" Object)
         (Assert "app builder WPO runtime libraries"
                 [[shen-scheme runtime]]
                 RuntimeLibraries)
         (shen-scheme.load-compiled Object)
         (Assert "app builder initializer order"
                 [1 12]
                 (value *native-app-init-events*))
         (Assert "app builder cross-module call" 52 (eval [native-app-main 41]))
         (Assert "app builder direct module call" 42 (eval [native-app-direct 41]))
         (Assert "app builder runtime global call" 3 (eval [native-app-length [cons 1 [cons 2 [cons 3 []]]]]))
         (Assert "app builder absvector fallback" true (eval [native-app-absvector?]))
         (Assert "app builder generic equality fallback" true (eval [native-app-list-equal?]))
         (Assert "app builder static global fallback" true (eval [native-app-sysfunc?])))))

(define native-test.run-module-app-builder
  -> (let BaseSource "_build/native-tests/module-app-base.shen"
          BaseUpdatedSource "_build/native-tests/module-app-base-updated.shen"
          MainSource "_build/native-tests/module-app-main.shen"
          PrivateCallSource "_build/native-tests/module-app-private-call.shen"
          BaseDeclaration "_build/native-tests/native.test.app-base.shenmod"
          MainDeclaration "_build/native-tests/native.test.app-main.shenmod"
          PrivateCallDeclaration "_build/native-tests/native.test.app-private-call.shenmod"
          MissingRequireDeclaration "_build/native-tests/native.test.app-missing-require.shenmod"
          BadExportDeclaration "_build/native-tests/native.test.app-bad-export.shenmod"
          Object "_build/native-tests/module-app-wpo.so"
          Assert (/. Label Expected Actual
                    (native-test.assert-equal Label Expected Actual))
       (do
         (native-test.write-file BaseSource
                                 (native-test.module-app-base-source))
         (native-test.write-file BaseUpdatedSource
                                 (native-test.module-app-base-updated-source))
         (native-test.write-file MainSource
                                 (native-test.module-app-main-source))
         (native-test.write-file
          PrivateCallSource
          (native-test.module-app-private-call-source))
         (native-test.write-file
          BaseDeclaration
          (native-test.module-app-base-declaration-source BaseSource))
         (native-test.write-file
          MainDeclaration
          (native-test.module-app-main-declaration-source MainSource))
         (native-test.write-file
          PrivateCallDeclaration
          (native-test.module-app-private-call-declaration-source
           PrivateCallSource))
         (native-test.write-file
          MissingRequireDeclaration
          (native-test.module-app-missing-require-declaration-source MainSource))
         (native-test.write-file
          BadExportDeclaration
          (native-test.module-app-bad-export-declaration-source MainSource))
         (let Result (shen-scheme.build-module-app/wpo
                      MainDeclaration
                      "_build/native-tests"
                      Object)
              BuiltObject (hd Result)
              RuntimeLibraries (hd (tl Result))
          (do
            (Assert "module app builder object" Object BuiltObject)
            (Assert "module app builder WPO runtime libraries"
                    [[shen-scheme runtime]]
                    RuntimeLibraries)
            (shen-scheme.load-compiled Object)
            (Assert "module app initializer order"
                    [10 11]
                    (value *native-module-app-init-events*))
            (Assert "module app cross-module direct call"
                    42
                    (eval [native-module-app-main 32]))
            (Assert "module app private dependency hidden"
                    unavailable
                    (trap-error (eval [native-module-app-private 1])
                                (/. E unavailable)))
            (load BaseUpdatedSource)
            (Assert "module app dependency redefined"
                    1001
                    (eval [native-module-app-base 1]))
            (Assert "module app keeps imported dependency binding"
                    42
                    (eval [native-module-app-main 32]))
            (Assert "module app missing dependency declaration fails"
                    failed
                    (trap-error
                     (shen-scheme.build-module-app/wpo
                      MissingRequireDeclaration
                      "_build/native-tests"
                      "_build/native-tests/module-app-missing-require.so")
                     (/. E failed)))
            (Assert "module app unknown export fails"
                    failed
                    (trap-error
                     (shen-scheme.build-module-app/wpo
                      BadExportDeclaration
                      "_build/native-tests"
                      "_build/native-tests/module-app-bad-export.so")
                     (/. E failed)))
            (let PrivateResult
                 (shen-scheme.build-module-app/wpo
                  PrivateCallDeclaration
                  "_build/native-tests"
                  "_build/native-tests/module-app-private-call.so")
              (do
                (shen-scheme.load-compiled (hd PrivateResult))
                (Assert "module app private dependency stays unavailable"
                        unavailable
                        (trap-error
                         (eval [native-module-app-private-probe 1])
                         (/. E unavailable))))))))))

(define native-test.run
  -> (do
    (native-test.run-refactor-regressions)
    (native-test.compile-load
      "tests/native/simple.shen"
      "_build/native-tests/api-simple.so"
      "_build/native-tests/api-simple.scm")
    (native-test.assert-simple "api first load" 42 8 15 [2 3 4])
    (native-test.compile-load
      "tests/native/simple-updated.shen"
      "_build/native-tests/api-simple-updated.so"
      "_build/native-tests/api-simple-updated.scm")
    (native-test.assert-simple "api reload" 52 18 115 [12 13 14])
    (native-test.run-direct-compile)
    (native-test.run-prolog)
    (native-test.run-package-effects)
    (native-test.run-profiles)
    (native-test.run-module-declarations)
    (native-test.run-nested-module-graph)
    (native-test.run-module-source-typechecking)
    (native-test.run-private-dependency-arity)
    (native-test.run-dependency-package-metadata)
    (native-test.run-compatible-redefinition)
    (native-test.run-sealed-redefinition)
    (native-test.run-app-builder)
    (native-test.run-module-app-builder)
    (native-test.run-core-regressions)
    (native-test.run-module-dependency-regressions)))

(load "scripts/run-native-core-regression-tests.shen")
(load "scripts/run-native-module-regression-tests.shen")
(native-test.run)
