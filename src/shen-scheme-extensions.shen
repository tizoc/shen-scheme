\* Copyright (c) 2012-2021 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(define shen-scheme.handle-launcher-result
  [error Message] -> (do (shen.x.launcher.default-handle-result
                            [error Message])
                         ((foreign scm.exit) 1))
  [unknown-arguments | Rest]
  -> (do (shen.x.launcher.default-handle-result
            [unknown-arguments | Rest])
         ((foreign scm.exit) 1))
  Other -> (shen.x.launcher.default-handle-result Other))

(define shen-scheme.profile-help-text
  -> "Profiles:
  release  Safe optimized native compilation. This is the default.
  debug    Keeps debug and source information.
  wpo      Enables Chez WPO sidecar generation; app builds produce WPO output.
  unsafe   Uses Chez optimize-level 3.

--profile unsafe uses Chez optimize-level 3. Chez generates unsafe code at this
level. Incorrect type or bounds assumptions can cause crashes or memory
corruption. Use only for trusted, well-tested code.
")

(define shen-scheme.help-text
  Exe -> (@s (shen.x.launcher.help-text Exe)
"

    compile <SOURCE> -o <OBJECT> [--emit-scheme <SCHEME>] [--mode compatible|sealed] [--profile release|debug|wpo|unsafe]
        Compiles a supported Shen source file to a Chez object file.

    load-compiled <OBJECT>
        Loads an object produced by a native compilation command.

    compile-module <DECLARATION> -o <OBJECT> [--emit-scheme <SCHEME>] [--module-dir <DIR>]
        Compiles a Shen-readable native module declaration to a Chez object file.

    load-module <DECLARATION> --module-dir <DIR>
        Loads a compiled native module and its required modules from DIR.

    build-app <MAIN> [--module <SOURCE> ...] -o <OBJECT> [--wpo] [--profile release|debug|wpo|unsafe]
        Builds a Shen application object from generated Chez libraries.
        WPO incorporates the generated application libraries; the resulting
        object still requires the matching Shen/Scheme runtime.

    build-module-app <DECLARATION> --module-dir <DIR> -o <OBJECT> [--wpo] [--profile release|debug|wpo|unsafe]
        Builds an application object from a closed native module declaration graph."))

(define shen-scheme.compile-help-text
  Exe -> (@s "Usage: " Exe
             " compile <SOURCE> -o <OBJECT> [--emit-scheme <SCHEME>] [--mode compatible|sealed] [--profile release|debug|wpo|unsafe]

Compiles a Shen source file to a Chez object file. Top-level package forms and
arbitrary Shen expression effects are supported. Definitions are hoisted;
effects, including effects inside packages, preserve their relative source
order and execute after definitions and metadata when the object is loaded.
Foreign Scheme define, import, module, and library forms are not supported as
native initializers.

Compatible mode is the default. Sealed mode binds intra-file calls locally and
therefore does not observe later redefinitions of same-file helper functions.
By default, generated Scheme forms are compiled directly. Use --emit-scheme to
write the generated Scheme file for debugging before compiling it.
"
             (shen-scheme.profile-help-text)))

(define shen-scheme.load-compiled-help-text
  Exe -> (@s "Usage: " Exe
             " load-compiled <OBJECT>

Loads an object produced by compile, compile-module, build-app, or
build-module-app.
"))

(define shen-scheme.compile-module-help-text
  Exe -> (@s "Usage: " Exe
             " compile-module <DECLARATION> -o <OBJECT> [--emit-scheme <SCHEME>] [--module-dir <DIR>]

Compiles a portable shen.module version 1 declaration to a Chez object file.

The declaration file is parsed as raw Shen data without macro expansion. The
portable core supports ordered source lists, feature requirements, and
static dependencies. Shen/Scheme compilation settings live in the shen/scheme
extension. Standalone module compilation requires sealed mode for explicit
exports. With --module-dir, symbolic requires are resolved as
<DIR>/<module-name>.shenmod and analyzed from source without loading their
compiled objects, installing ordinary definitions, or running initializers.
Relative source paths are resolved from the declaration file's directory.
Dependency macros must be self-contained or use helpers already loaded in the
compiler, and their transformer names must not collide with live bindings.
Foreign Scheme definition-context forms are not supported as native
initializers.
"))

(define shen-scheme.load-module-help-text
  Exe -> (@s "Usage: " Exe
             " load-module <DECLARATION> --module-dir <DIR>

Loads a compiled native module and its transitive required modules from DIR.
Module names resolve as <DIR>/<module-name>.shenmod and
<DIR>/<module-name>.so.
"))

(define shen-scheme.build-app-help-text
  Exe -> (@s "Usage: " Exe
             " build-app <MAIN> [--module <SOURCE> ...] -o <OBJECT> [--wpo] [--profile release|debug|wpo|unsafe]

Builds an application object from supported Shen source files.

Module sources are compiled before MAIN. Later sources can statically call
functions from earlier sources. Compile-time forms are available while the app
is built, but dependency macros cannot call ordinary helpers defined only in an
earlier source and their transformer names must not collide with live bindings.
Compile-time forms are not replayed when a raw source app object is loaded.
Foreign Scheme definition-context forms are not supported as native
initializers.
With --wpo, Chez whole-program optimization incorporates all generated
application libraries. The object still requires the matching Shen/Scheme
runtime.
"
             (shen-scheme.profile-help-text)))

(define shen-scheme.build-module-app-help-text
  Exe -> (@s "Usage: " Exe
             " build-module-app <DECLARATION> --module-dir <DIR> -o <OBJECT> [--wpo] [--profile release|debug|wpo|unsafe]

Builds an application object from a closed native module declaration graph.
Dependencies are resolved from --module-dir as <DIR>/<module-name>.shenmod.
Descriptor exports form the static native boundary between generated libraries,
and descriptor compile-time metadata is replayed when the app is loaded.
Dependencies use static app bindings. The command-line profile controls the app
build; descriptor mode and profile apply to standalone compile-module builds.
Direct requirements that export the same function are rejected.
The graph is analyzed in topological order as one compiler environment.
Dependency macros must be self-contained or use helpers already loaded in the
compiler, and their transformer names must not collide with live bindings.
Foreign Scheme definition-context forms are not supported as native
initializers.
"
             (shen-scheme.profile-help-text)))

(define shen-scheme.parse-compile-args
  [Source | Args] -> (shen-scheme.parse-compile-options Args Source (fail) (fail) compatible release)
  _ -> [error])

(define shen-scheme.parse-compile-options
  [] Source Object Scheme Mode Profile -> [ok Source Object Scheme Mode Profile]
  ["-o" Object | Rest] Source _ Scheme Mode Profile
  -> (shen-scheme.parse-compile-options Rest Source Object Scheme Mode Profile)
  ["--emit-scheme" Scheme | Rest] Source Object _ Mode Profile
  -> (shen-scheme.parse-compile-options Rest Source Object Scheme Mode Profile)
  ["--mode" Mode | Rest] Source Object Scheme _ Profile
  -> (shen-scheme.parse-compile-options Rest Source Object Scheme Mode Profile)
  ["--profile" Profile | Rest] Source Object Scheme Mode _
  -> (shen-scheme.parse-compile-options Rest Source Object Scheme Mode Profile)
  [Arg | _] _ _ _ _ _ -> [error Arg])

(define shen-scheme.compile-command*
  Exe [ok _ Object _ _ _] -> [error (shen-scheme.compile-help-text Exe)]
    where (= Object (fail))
  _ [ok Source Object Scheme Mode Profile]
  -> (let Options (shen-scheme.native-compile-profile-options Profile)
       (if (= Scheme (fail))
           (do (shen-scheme.compile-file/options/mode Source Object Options Mode)
               [success])
           (do (shen-scheme.compile-file/emit/options/mode Source Object Scheme Options Mode)
               [success])))
  Exe _ -> [error (shen-scheme.compile-help-text Exe)])

(define shen-scheme.compile-command
  Exe ["--help"] -> [show-help (shen-scheme.compile-help-text Exe)]
  Exe Args -> (shen-scheme.compile-command* Exe (shen-scheme.parse-compile-args Args)))

(define shen-scheme.load-compiled-command
  Exe ["--help"] -> [show-help (shen-scheme.load-compiled-help-text Exe)]
  _ [Object] -> (do (shen-scheme.load-compiled Object)
                    [success])
  Exe _ -> [error (shen-scheme.load-compiled-help-text Exe)])

(define shen-scheme.parse-compile-module-args
  [Declaration | Args] -> (shen-scheme.parse-compile-module-options
                           Args Declaration (fail) (fail) (fail))
  _ -> [error])

(define shen-scheme.parse-compile-module-options
  [] Declaration Object Scheme ModuleDir
  -> [ok Declaration Object Scheme ModuleDir]
  ["-o" Object | Rest] Declaration _ Scheme ModuleDir
  -> (shen-scheme.parse-compile-module-options Rest Declaration Object Scheme ModuleDir)
  ["--emit-scheme" Scheme | Rest] Declaration Object _ ModuleDir
  -> (shen-scheme.parse-compile-module-options Rest Declaration Object Scheme ModuleDir)
  ["--module-dir" ModuleDir | Rest] Declaration Object Scheme _
  -> (shen-scheme.parse-compile-module-options Rest Declaration Object Scheme ModuleDir)
  [Arg | _] _ _ _ _ -> [error Arg])

(define shen-scheme.compile-module-command*
  Exe [ok _ Object _ _] -> [error (shen-scheme.compile-module-help-text Exe)]
    where (= Object (fail))
  _ [ok Declaration Object Scheme ModuleDir]
  -> (do (shen-scheme.compile-module Declaration Object)
         [success])
    where (and (= Scheme (fail)) (= ModuleDir (fail)))
  _ [ok Declaration Object Scheme ModuleDir]
  -> (do (shen-scheme.compile-module/emit Declaration Object Scheme)
         [success])
    where (= ModuleDir (fail))
  _ [ok Declaration Object Scheme ModuleDir]
  -> (do (shen-scheme.compile-module/in-dir Declaration Object ModuleDir)
         [success])
    where (= Scheme (fail))
  _ [ok Declaration Object Scheme ModuleDir]
  -> (do (shen-scheme.compile-module/emit/in-dir Declaration Object Scheme ModuleDir)
         [success])
  Exe _ -> [error (shen-scheme.compile-module-help-text Exe)])

(define shen-scheme.compile-module-command
  Exe ["--help"] -> [show-help (shen-scheme.compile-module-help-text Exe)]
  Exe Args -> (shen-scheme.compile-module-command*
               Exe (shen-scheme.parse-compile-module-args Args)))

(define shen-scheme.load-module-command
  Exe ["--help"] -> [show-help (shen-scheme.load-module-help-text Exe)]
  _ [Declaration "--module-dir" ModuleDir]
  -> (do (shen-scheme.load-module Declaration ModuleDir)
         [success])
  Exe _ -> [error (shen-scheme.load-module-help-text Exe)])

(define shen-scheme.parse-build-app-args
  [] Modules Object Wpo Profile -> [ok (reverse Modules) Object Wpo Profile]
  ["--module" Module | Rest] Modules Object Wpo Profile
  -> (shen-scheme.parse-build-app-args Rest [Module | Modules] Object Wpo Profile)
  ["-o" Object | Rest] Modules _ Wpo Profile
  -> (shen-scheme.parse-build-app-args Rest Modules Object Wpo Profile)
  ["--wpo" | Rest] Modules Object _ Profile
  -> (shen-scheme.parse-build-app-args Rest Modules Object true Profile)
  ["--profile" Profile | Rest] Modules Object Wpo _
  -> (shen-scheme.parse-build-app-args Rest Modules Object Wpo Profile)
  [Arg | _] _ _ _ _ -> [error Arg])

(define shen-scheme.build-app-command*
  Exe _ [ok _ Object _ _] -> [error (shen-scheme.build-app-help-text Exe)]
    where (= Object (fail))
  _ Main [ok Modules Object Wpo Profile]
  -> (do (shen-scheme.build-app/emit/options
          Main Modules Object "_build/native-app"
          (shen-scheme.native-compile-profile-options Profile) Wpo)
         [success])
  Exe _ _ -> [error (shen-scheme.build-app-help-text Exe)])

(define shen-scheme.build-app-command
  Exe ["--help"] -> [show-help (shen-scheme.build-app-help-text Exe)]
  Exe [Main | Args] -> (shen-scheme.build-app-command*
                        Exe Main
                        (shen-scheme.parse-build-app-args Args [] (fail) false release))
  Exe _ -> [error (shen-scheme.build-app-help-text Exe)])

(define shen-scheme.parse-build-module-app-args
  [] ModuleDir Object Wpo Profile -> [ok ModuleDir Object Wpo Profile]
  ["--module-dir" ModuleDir | Rest] _ Object Wpo Profile
  -> (shen-scheme.parse-build-module-app-args Rest ModuleDir Object Wpo Profile)
  ["-o" Object | Rest] ModuleDir _ Wpo Profile
  -> (shen-scheme.parse-build-module-app-args Rest ModuleDir Object Wpo Profile)
  ["--wpo" | Rest] ModuleDir Object _ Profile
  -> (shen-scheme.parse-build-module-app-args Rest ModuleDir Object true Profile)
  ["--profile" Profile | Rest] ModuleDir Object Wpo _
  -> (shen-scheme.parse-build-module-app-args Rest ModuleDir Object Wpo Profile)
  [Arg | _] _ _ _ _ -> [error Arg])

(define shen-scheme.build-module-app-command*
  Exe _ [ok ModuleDir _ _ _] -> [error (shen-scheme.build-module-app-help-text Exe)]
    where (= ModuleDir (fail))
  Exe _ [ok _ Object _ _] -> [error (shen-scheme.build-module-app-help-text Exe)]
    where (= Object (fail))
  _ Declaration [ok ModuleDir Object true Profile]
  -> (do (shen-scheme.build-module-app/wpo/profile Declaration ModuleDir Object Profile)
         [success])
  _ Declaration [ok ModuleDir Object false Profile]
  -> (do (shen-scheme.build-module-app/profile Declaration ModuleDir Object Profile)
         [success])
  Exe _ _ -> [error (shen-scheme.build-module-app-help-text Exe)])

(define shen-scheme.build-module-app-command
  Exe ["--help"] -> [show-help (shen-scheme.build-module-app-help-text Exe)]
  Exe [Declaration | Args] -> (shen-scheme.build-module-app-command*
                               Exe Declaration
                               (shen-scheme.parse-build-module-app-args
                                Args (fail) (fail) false release))
  Exe _ -> [error (shen-scheme.build-module-app-help-text Exe)])

(define shen-scheme.run-shen
  [Exe "--help"] -> (shen-scheme.handle-launcher-result [show-help (shen-scheme.help-text Exe)])
  [Exe "compile" | Args]
  -> (shen-scheme.handle-launcher-result (shen-scheme.compile-command Exe Args))
  [Exe "load-compiled" | Args]
  -> (shen-scheme.handle-launcher-result (shen-scheme.load-compiled-command Exe Args))
  [Exe "compile-module" | Args]
  -> (shen-scheme.handle-launcher-result (shen-scheme.compile-module-command Exe Args))
  [Exe "load-module" | Args]
  -> (shen-scheme.handle-launcher-result (shen-scheme.load-module-command Exe Args))
  [Exe "build-app" | Args]
  -> (shen-scheme.handle-launcher-result (shen-scheme.build-app-command Exe Args))
  [Exe "build-module-app" | Args] -> (shen-scheme.handle-launcher-result
                                      (shen-scheme.build-module-app-command Exe Args))
  Args -> (shen-scheme.handle-launcher-result (shen.x.launcher.launch-shen Args)))

(define shen-scheme.find-library
  Name -> ((foreign scm.string-append) ((foreign scm.get-shen-scheme-home-path)) "/libraries/" Name))

(define thread
  Lazy -> ((foreign scm.fork-thread) Lazy))
