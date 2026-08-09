\* Copyright (c) 2012-2021 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(define native-test.run-core-regressions
  -> (let Dir "_build/native-tests"
          ForwardCaller "_build/native-tests/core-forward-caller.shen"
          ForwardTarget "_build/native-tests/core-forward-target.shen"
          ForwardCompatible "_build/native-tests/core-forward-compatible.shenmod"
          ForwardSealed "_build/native-tests/core-forward-sealed.shenmod"
          Duplicate "_build/native-tests/core-duplicate.shen"
          DuplicateModule "_build/native-tests/core-duplicate.shenmod"
          ArityBase "_build/native-tests/core-arity-base.shen"
          ArityChanged "_build/native-tests/core-arity-changed.shen"
          Package "_build/native-tests/core-package.shen"
          PackageChain "_build/native-tests/core-package-chain.shen"
          SystemDefinition "_build/native-tests/core-system-definition.shen"
          Macro "_build/native-tests/core-macro.shen"
          Assert (/. L E A (native-test.assert-equal L E A))
       (do
         (native-test.write-file
          ForwardCaller
          "(define native-core-forward-call
  X -> (native-core-forward-target X 2))
")
         (native-test.write-file
          ForwardTarget
          "(define native-core-forward-target
  X Y -> (+ X Y))
")
         (native-test.write-file
          ForwardCompatible
          (make-string "(shen.module
  (version 1)
  (name native.test.core-forward-compatible)
  (sources tc- ~S ~S)
  (extension shen/scheme
    (mode compatible)))
"
                       (native-test.basename ForwardCaller)
                       (native-test.basename ForwardTarget)))
         (native-test.write-file
          ForwardSealed
          (make-string "(shen.module
  (version 1)
  (name native.test.core-forward-sealed)
  (sources tc- ~S ~S)
  (extension shen/scheme
    (mode sealed)
    (exports native-core-forward-call)))
"
                       (native-test.basename ForwardCaller)
                       (native-test.basename ForwardTarget)))
         (shen-scheme.compile-module
          ForwardCompatible
          "_build/native-tests/core-forward-compatible.so")
         (shen-scheme.load-compiled
          "_build/native-tests/core-forward-compatible.so")
         (Assert "compatible source set sees later arity"
                 42
                 (eval [native-core-forward-call 40]))
         (shen-scheme.compile-module
          ForwardSealed
          "_build/native-tests/core-forward-sealed.so")
         (shen-scheme.load-compiled
          "_build/native-tests/core-forward-sealed.so")
         (Assert "sealed source set sees later arity"
                 42
                 (eval [native-core-forward-call 40]))

         (native-test.write-file
         Duplicate
          "(define native-core-duplicate
  { number --> number }
  X -> (+ X 1))

(define native-core-duplicate
  X -> (+ X 2))

(define native-core-duplicate-call
  X -> (native-core-duplicate X))
")
         (shen-scheme.compile-file/mode
          Duplicate
          "_build/native-tests/core-duplicate-sealed.so"
          sealed)
         (shen-scheme.load-compiled
          "_build/native-tests/core-duplicate-sealed.so")
         (Assert "sealed duplicate definition keeps last"
                 7
                 (eval [native-core-duplicate-call 5]))
         (Assert "sealed duplicate preserves earlier signature"
                 true
                 (not (= []
                         (assoc native-core-duplicate
                                (value shen.*sigf*)))))
         (native-test.write-file
          DuplicateModule
          (make-string "(shen.module
  (version 1)
  (name native.test.core-duplicate)
  (sources tc- ~S)
  (extension shen/scheme
    (mode sealed)
    (exports native-core-duplicate-call)))
"
                       (native-test.basename Duplicate)))
         (shen-scheme.build-module-app
          DuplicateModule
          Dir
          "_build/native-tests/core-duplicate-app.so")
         (shen-scheme.load-compiled
          "_build/native-tests/core-duplicate-app.so")
         (Assert "module app duplicate definition keeps last"
                 7
                 (eval [native-core-duplicate-call 5]))

         (native-test.write-file
          ArityBase
          "(define native-core-arity
  X Y -> (+ X Y))
")
         (native-test.write-file
          ArityChanged
          "(define native-core-arity
  X -> (+ X 100))
")
         (load ArityBase)
         (let Before (arity native-core-arity)
           (do
             (shen-scheme.compile-file
              ArityChanged
              "_build/native-tests/core-arity-changed.so")
             (Assert "compile-only preserves live arity"
                     Before
                     (arity native-core-arity))
             (Assert "compile-only preserves live function calls"
                     42
                     (eval [native-core-arity 40 2]))))

         (native-test.write-file
          Package
          "(package native.core.regression.pkg
  [native-core-package-public]

(define native-core-package-public
  X -> X))
")
         (Assert "compile package starts unregistered"
                 false
                 (package? native.core.regression.pkg))
         (shen-scheme.compile-file
          Package
          "_build/native-tests/core-package.so")
         (Assert "compile-only leaves package registry unchanged"
                 false
                 (package? native.core.regression.pkg))

         (native-test.write-file
          PackageChain
          "(package native.core.regression.first
  [native-core-package-first]

(define native-core-package-first
  X -> (+ X 1)))

(package native.core.regression.second
  (external native.core.regression.first)

(define second-private
  X -> (native-core-package-first X)))
")
         (shen-scheme.compile-file
          PackageChain
          "_build/native-tests/core-package-chain.so")
         (Assert "compile package chain leaves first package unregistered"
                 false
                 (package? native.core.regression.first))
         (Assert "compile package chain leaves second package unregistered"
                 false
                 (package? native.core.regression.second))
         (shen-scheme.load-compiled
          "_build/native-tests/core-package-chain.so")
         (Assert "later package sees earlier package externals"
                 42
                 (eval [native.core.regression.second.second-private 41]))

         (native-test.write-file
          SystemDefinition
          "(define +
  X Y -> X)
")
         (Assert "native compile rejects same-arity system definition"
                 failed
                 (trap-error
                  (shen-scheme.compile-file
                   SystemDefinition
                   "_build/native-tests/core-system-definition.so")
                  (/. E failed)))

         (native-test.write-file
          Macro
          "(defmacro native-core-synonym-macro
  [native-core-synonym] -> [synonyms native-core-count number])

(native-core-synonym)
")
         (let Macros (value *macros*)
              SigF (value shen.*sigf*)
              Synonyms (value shen.*synonyms*)
              Demod (value shen.*demodulation-function*)
              Datatypes (value shen.*datatypes*)
              AllDatatypes (value shen.*alldatatypes*)
              UserDefs (value shen.*userdefs*)
           (do
             (shen-scheme.compile-file
              Macro
              "_build/native-tests/core-macro.so")
             (Assert "compile-only restores macros"
                     Macros (value *macros*))
             (Assert "compile-only restores declarations"
                     SigF (value shen.*sigf*))
             (Assert "compile-only restores synonyms"
                     Synonyms (value shen.*synonyms*))
             (Assert "compile-only restores type demodulation"
                     Demod (value shen.*demodulation-function*))
             (Assert "compile-only restores datatypes"
                     Datatypes (value shen.*datatypes*))
             (Assert "compile-only restores all datatypes"
                     AllDatatypes (value shen.*alldatatypes*))
             (Assert "compile-only restores user definitions"
                     UserDefs (value shen.*userdefs*))))
         (shen-scheme.load-compiled
          "_build/native-tests/core-macro.so")
         (Assert "macro-generated synonym is replayed"
                 true
                 (element? native-core-count
                           (value shen.*synonyms*))))))
