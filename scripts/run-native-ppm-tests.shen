\* Copyright (c) 2012-2021 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(define native-test.ppm-handler-source
  -> "(define @native-test-ppm-two
  A B -> (@p A B))

(package null []

(define native-test-ppm-project
  Self AddTest Bind A B
  -> (do (AddTest [tuple? Self])
         (Bind A [fst Self])
         (Bind B [snd Self])))

(define native-test-ppm-handler
  Self AddTest Bind [@native-test-ppm-two A B]
  -> (native-test-ppm-project Self AddTest Bind A B)
  _ _ _ _ -> (fail))

(shen.x.programmable-pattern-matching.register-handler
 native-test-ppm-handler))
")

(define native-test.ppm-later-source
  -> "(define native-test-ppm-later
  (@native-test-ppm-two A B) -> [A B]
  _ -> no)
")

(define native-test.ppm-macro-source
  -> "(defmacro native-test-ppm-ordered-macro
  (@native-test-ppm-two A B) -> [quote [A B]])
")

(define native-test.ppm-required-source
  -> "(define native-test-ppm-required
  (@native-test-ppm-two A B) -> [A B]
  _ -> no)
")

(define native-test.ppm-same-file-source
  -> (@s (native-test.ppm-handler-source)
         "
(define native-test-ppm-same-file
  (@native-test-ppm-two A B) -> [A B]
  _ -> no)
"))

(define native-test.ppm-invalid-source
  -> "(define native-test-ppm-valid-before-error
  (@native-test-ppm-two A B) -> [A B]
  _ -> no)

(define native-test-ppm-invalid
  (@native-test-ppm-unknown X) -> X
  _ -> no)
")

(define native-test.ppm-unregister-source
  -> "(shen.x.programmable-pattern-matching.unregister-handler
 native-test-ppm-handler)

(define native-test-ppm-before-unregister
  (@native-test-ppm-two A B) -> [A B]
  _ -> no)
")

(define native-test.ppm-fresh-source
  F -> (make-string "(define ~A
  (@native-test-ppm-two A B) -> [A B]
  _ -> no)
" F))

(define native-test.ppm-shadow-source
  -> "(define native-test-ppm-later
  X -> stale)

(define native-test-ppm-required
  X -> stale)
")

(define native-test.ppm-declaration
  Name Sources Requires Exports Metadata
  -> (make-string "(shen.module
  (version 1)
  (name ~A)
  ~A
  (sources tc- ~A)
  (extension shen/scheme
    (mode sealed)
    (exports ~A)
    (metadata ~A)))
"
                  Name
                  (if (= Requires [])
                      ""
                      (make-string "(requires ~A)" Requires))
                  (native-test.ppm-source-list Sources)
                  Exports
                  Metadata))

(define native-test.ppm-source-list
  [S] -> (make-string "~S" S)
  [S | Ss] -> (make-string "~S ~A" S (native-test.ppm-source-list Ss)))

(define native-test.ppm-handler-names
  -> (value shen.x.programmable-pattern-matching.*pattern-handlers-reg*))

(define native-test.ppm-handlers
  -> (value shen.x.programmable-pattern-matching.*pattern-handlers*))

(define native-test.ppm-call-private-handler
  -> (native-test-ppm-handler
      (@p 1 2)
      (/. X true)
      (/. X (/. Y bound))
      [@native-test-ppm-two first second]))

(define native-test.ppm-cleanup
  -> (do (if (element? native-test-ppm-handler
                       (native-test.ppm-handler-names))
             (shen.x.programmable-pattern-matching.unregister-handler
              native-test-ppm-handler)
             skip)
         (if (= [] (assoc native-test-ppm-ordered-macro (value *macros*)))
             skip
             (undefmacro native-test-ppm-ordered-macro))))

(define native-test.run-native-ppm
  -> (let Dir "_build/native-tests"
          ObjectDir "_build/native-tests/module-objects"
          HandlerSource "_build/native-tests/native-ppm-handler.shen"
          LaterSource "_build/native-tests/native-ppm-later.shen"
          MacroSource "_build/native-tests/native-ppm-macro.shen"
          RequiredSource "_build/native-tests/native-ppm-required.shen"
          SameFileSource "_build/native-tests/native-ppm-same-file.shen"
          InvalidSource "_build/native-tests/native-ppm-invalid.shen"
          UnregisterSource "_build/native-tests/native-ppm-unregister.shen"
          CompiletimeFreshSource
          "_build/native-tests/native-ppm-compiletime-fresh.shen"
          RuntimeFreshSource
          "_build/native-tests/native-ppm-runtime-fresh.shen"
          ShadowSource "_build/native-tests/native-ppm-shadow.shen"
          ProviderDeclaration
          "_build/native-tests/native.test.ppm-provider.shenmod"
          RuntimeDeclaration
          "_build/native-tests/native.test.ppm-runtime.shenmod"
          RootDeclaration
          "_build/native-tests/native.test.ppm-root.shenmod"
          SameFileDeclaration
          "_build/native-tests/native.test.ppm-same-file.shenmod"
          InvalidDeclaration
          "_build/native-tests/native.test.ppm-invalid.shenmod"
          UnregisterDeclaration
          "_build/native-tests/native.test.ppm-unregister.shenmod"
          ProviderObject
          "_build/native-tests/module-objects/native.test.ppm-provider.so"
          RuntimeObject
          "_build/native-tests/module-objects/native.test.ppm-runtime.so"
          RootObject
          "_build/native-tests/module-objects/native.test.ppm-root.so"
          SameFileObject
          "_build/native-tests/module-objects/native.test.ppm-same-file.so"
          InvalidObject
          "_build/native-tests/module-objects/native.test.ppm-invalid.so"
          UnregisterObject
          "_build/native-tests/module-objects/native.test.ppm-unregister.so"
          AppObject "_build/native-tests/native-ppm-app.so"
          Assert (/. Label Expected Actual
                    (native-test.assert-equal Label Expected Actual))
       (do
         (native-test.ppm-cleanup)
         (let InitialNames (native-test.ppm-handler-names)
              InitialHandlers (native-test.ppm-handlers)
           (do
             (native-test.write-file HandlerSource
                                     (native-test.ppm-handler-source))
             (native-test.write-file LaterSource
                                     (native-test.ppm-later-source))
             (native-test.write-file MacroSource
                                     (native-test.ppm-macro-source))
             (native-test.write-file RequiredSource
                                     (native-test.ppm-required-source))
             (native-test.write-file SameFileSource
                                     (native-test.ppm-same-file-source))
             (native-test.write-file InvalidSource
                                     (native-test.ppm-invalid-source))
             (native-test.write-file UnregisterSource
                                     (native-test.ppm-unregister-source))
             (native-test.write-file
              CompiletimeFreshSource
              (native-test.ppm-fresh-source
               native-test-ppm-after-compiletime-load))
             (native-test.write-file
              RuntimeFreshSource
              (native-test.ppm-fresh-source
               native-test-ppm-after-runtime-load))
             (native-test.write-file ShadowSource
                                     (native-test.ppm-shadow-source))
             (native-test.write-file
              ProviderDeclaration
              (native-test.ppm-declaration
               native.test.ppm-provider
               ["native-ppm-handler.shen" "native-ppm-later.shen"
                "native-ppm-macro.shen"]
               []
               "@native-test-ppm-two native-test-ppm-later"
               "runtime compiletime"))
             (native-test.write-file
              RuntimeDeclaration
              (native-test.ppm-declaration
               native.test.ppm-runtime
               ["native-ppm-handler.shen" "native-ppm-later.shen"]
               []
               "@native-test-ppm-two native-test-ppm-later"
               runtime))
             (native-test.write-file
              RootDeclaration
              (native-test.ppm-declaration
               native.test.ppm-root
               ["native-ppm-required.shen"]
               native.test.ppm-provider
               native-test-ppm-required
               "runtime compiletime"))
             (native-test.write-file
              SameFileDeclaration
              (native-test.ppm-declaration
               native.test.ppm-same-file
               ["native-ppm-same-file.shen"]
               []
               native-test-ppm-same-file
               "runtime compiletime"))
             (native-test.write-file
              InvalidDeclaration
              (native-test.ppm-declaration
               native.test.ppm-invalid
               ["native-ppm-handler.shen" "native-ppm-invalid.shen"]
               []
               native-test-ppm-valid-before-error
               "runtime compiletime"))
             (native-test.write-file
              UnregisterDeclaration
              (native-test.ppm-declaration
               native.test.ppm-unregister
               ["native-ppm-handler.shen" "native-ppm-unregister.shen"]
               []
               "@native-test-ppm-two native-test-ppm-before-unregister"
               "runtime compiletime"))
             (map (function native-test.delete-file-if-exists)
                  [ProviderObject RuntimeObject RootObject SameFileObject
                   InvalidObject UnregisterObject AppObject])
             (Assert "native PPM is inactive in its defining source"
                     failed
                     (trap-error
                      (shen-scheme.compile-module
                       SameFileDeclaration SameFileObject)
                      (/. X failed)))
             (Assert "failed native PPM compile restores handler names"
                     InitialNames
                     (native-test.ppm-handler-names))
             (Assert "failed native PPM compile restores handlers"
                     InitialHandlers
                     (native-test.ppm-handlers))
             (Assert "native PPM failure after registration is isolated"
                     failed
                     (trap-error
                      (shen-scheme.compile-module
                       InvalidDeclaration InvalidObject)
                      (/. X failed)))
             (Assert "later native PPM failure restores handler names"
                     InitialNames
                     (native-test.ppm-handler-names))
             (Assert "later native PPM failure restores handlers"
                     InitialHandlers
                     (native-test.ppm-handlers))
             (shen-scheme.build-module-app
              RootDeclaration Dir AppObject)
             (Assert "native PPM app build restores handler names"
                     InitialNames
                     (native-test.ppm-handler-names))
             (Assert "native PPM app build restores handlers"
                     InitialHandlers
                     (native-test.ppm-handlers))
             (shen-scheme.compile-module
              ProviderDeclaration ProviderObject)
             (shen-scheme.compile-module
              RuntimeDeclaration RuntimeObject)
             (shen-scheme.compile-module/in-dir
              RootDeclaration RootObject Dir)
             (shen-scheme.compile-module
              UnregisterDeclaration UnregisterObject)
             (Assert "native PPM compile restores handler names"
                     InitialNames
                     (native-test.ppm-handler-names))
             (Assert "native PPM compile restores handlers"
                     InitialHandlers
                     (native-test.ppm-handlers))
             (shen-scheme.native-load-compiled-for-compilation
              RuntimeObject)
             (Assert "runtime-only native PPM metadata stays inactive"
                     InitialNames
                     (native-test.ppm-handler-names))
             (shen-scheme.native-load-compiled-for-compilation
              ProviderObject)
             (Assert "native PPM compiletime metadata registers handler"
                     [native-test-ppm-handler | InitialNames]
                     (native-test.ppm-handler-names))
             (Assert "native PPM compiletime effects retain source order"
                     [quote [1 2]]
                     (native-test-ppm-ordered-macro (@p 1 2)))
             (load CompiletimeFreshSource)
             (native-test.ppm-cleanup)
             (Assert "native PPM compiletime handler compiles new source"
                     [1 2]
                     (eval [native-test-ppm-after-compiletime-load [@p 1 2]]))
             (Assert "native PPM handler remains private"
                     unavailable
                     (trap-error (native-test.ppm-call-private-handler)
                                 (/. X unavailable)))
             (shen-scheme.load-compiled RuntimeObject)
             (Assert "native PPM runtime initializer registers handler"
                     [native-test-ppm-handler | InitialNames]
                     (native-test.ppm-handler-names))
             (load RuntimeFreshSource)
             (native-test.ppm-cleanup)
             (Assert "native PPM runtime handler compiles new source"
                     [1 2]
                     (eval [native-test-ppm-after-runtime-load [@p 1 2]]))
             (load ShadowSource)
             (Assert "native PPM module matcher starts shadowed"
                     stale
                     (eval [native-test-ppm-required [@p 1 2]]))
             (shen-scheme.load-module RootDeclaration Dir ObjectDir)
             (Assert "native PPM later source matcher"
                     [1 2]
                     (eval [native-test-ppm-later [@p 1 2]]))
             (Assert "native PPM required module matcher"
                     [1 2]
                     (eval [native-test-ppm-required [@p 1 2]]))
             (native-test.ppm-cleanup)
             (load ShadowSource)
             (Assert "native PPM app matcher starts shadowed"
                     stale
                     (eval [native-test-ppm-required [@p 1 2]]))
             (shen-scheme.load-compiled AppObject)
             (Assert "native PPM module app matcher"
                     [1 2]
                     (eval [native-test-ppm-required [@p 1 2]]))
             (native-test.ppm-cleanup)
             (shen-scheme.load-compiled UnregisterObject)
             (Assert "native PPM unregister is deferred to its file boundary"
                     [1 2]
                     (eval [native-test-ppm-before-unregister [@p 1 2]]))
             (Assert "native PPM unregister replays at load"
                     InitialNames
                     (native-test.ppm-handler-names)))))))
