\* Copyright (c) 2012-2021 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(define native-test.run-module-dependency-regressions
  -> (let Dir "_build/native-tests"
          ObjectDir "_build/native-tests/module-regression-objects"
          BSource "_build/native-tests/module-precedence-b.shen"
          BMacroSource "_build/native-tests/module-precedence-b-macro.shen"
          ASource "_build/native-tests/module-precedence-a.shen"
          MainSource "_build/native-tests/module-precedence-main.shen"
          BDeclaration "_build/native-tests/native.test.precedence-b.shenmod"
          ADeclaration "_build/native-tests/native.test.precedence-a.shenmod"
          MainDeclaration "_build/native-tests/native.test.precedence-main.shenmod"
          BObject
          "_build/native-tests/module-regression-objects/native.test.precedence-b.so"
          AObject
          "_build/native-tests/module-regression-objects/native.test.precedence-a.so"
          MainObject
          "_build/native-tests/module-regression-objects/native.test.precedence-main.so"
          AppObject "_build/native-tests/module-precedence-app.so"
          Assert (/. Label Expected Actual
                    (native-test.assert-equal Label Expected Actual))
       (do
         (native-test.write-file
          BSource
          "(set *native-module-regression-effects*
     (+ (value *native-module-regression-effects*) 1))

(declare native-module-regression-shared
         [number --> number --> number])

(define native-module-regression-shared
  X Y -> (+ X Y))

(define native-module-regression-expand
  X -> [native-module-regression-shared X 2])
")
         (native-test.write-file
          BMacroSource
          "
(defmacro native-module-regression-call-shared-macro
  [native-module-regression-call-shared X]
  -> (native-module-regression-expand X))
")
         (native-test.write-file
          ASource
          "(define native-module-regression-shared
  X -> (+ X 100))
")
         (native-test.write-file
          MainSource
          "(define native-module-regression-main
  X -> (native-module-regression-call-shared X))
")
         (native-test.write-file
          BDeclaration
          (make-string "(shen.module
  (version 1)
  (name native.test.precedence-b)
  (sources tc- ~S ~S)
  (extension shen/scheme
    (mode sealed)
    (exports native-module-regression-shared)))
"
                       (native-test.basename BSource)
                       (native-test.basename BMacroSource)))
         (native-test.write-file
          ADeclaration
          (make-string "(shen.module
  (version 1)
  (name native.test.precedence-a)
  (requires native.test.precedence-b)
  (sources tc- ~S)
  (extension shen/scheme
    (mode sealed)
    (exports native-module-regression-shared)))
"
                       (native-test.basename ASource)))
         (native-test.write-file
          MainDeclaration
          (make-string "(shen.module
  (version 1)
  (name native.test.precedence-main)
  (requires native.test.precedence-a native.test.precedence-b)
  (sources tc- ~S)
  (extension shen/scheme
    (mode sealed)
    (exports native-module-regression-main)))
"
                       (native-test.basename MainSource)))
         (native-test.delete-file-if-exists BObject)
         (native-test.delete-file-if-exists AObject)
         (native-test.delete-file-if-exists MainObject)
         (native-test.delete-file-if-exists AppObject)
         (set *native-module-regression-effects* 0)
         (load ASource)
         (let InitialArity (arity native-module-regression-shared)
              InitialSignature (assoc native-module-regression-shared
                                      (value shen.*sigf*))
           (do
             (shen-scheme.compile-module/in-dir
              ADeclaration AObject Dir)
             (Assert "module compile needs no dependency object"
                     false
                     (shen-scheme.file-exists? BObject))
             (Assert "module compile skips dependency initializer"
                     0
                     (value *native-module-regression-effects*))
             (Assert "module compile leaves live dependency function"
                     140
                     (eval [native-module-regression-shared 40]))
             (shen-scheme.compile-module BDeclaration BObject)
             (shen-scheme.compile-module/in-dir
              MainDeclaration MainObject Dir)
             (Assert "module compile reapply skips dependency initializer"
                     0
                     (value *native-module-regression-effects*))
             (Assert "module dependency arities stay compiler-local"
                     InitialArity
                     (arity native-module-regression-shared))
             (Assert "module dependency signatures stay compiler-local"
                     InitialSignature
                     (assoc native-module-regression-shared
                            (value shen.*sigf*)))
             (Assert "module compile leaves live function binding"
                     140
                     (eval [native-module-regression-shared 40]))
             (Assert "module compile isolates private macro helper"
                     unavailable
                     (trap-error
                      (eval [native-module-regression-expand 40])
                      (/. E unavailable)))
             (Assert "module app rejects ambiguous direct exports"
                     failed
                     (trap-error
                      (shen-scheme.build-module-app
                       MainDeclaration Dir AppObject)
                      (/. E failed)))
             (Assert "module app build skips source initializer"
                     0
                     (value *native-module-regression-effects*))
             (set *native-module-regression-effects* 0)
             (shen-scheme.load-module MainDeclaration Dir ObjectDir)
             (Assert "module load does not repeat transitive initializer"
                     1
                     (value *native-module-regression-effects*))
             (Assert "module compile uses later dependency metadata"
                     42
                     (eval [native-module-regression-main 40])))))))
