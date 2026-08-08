\* Copyright (c) 2012-2021 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

\* How to use:

    (load "build.shen")
    (build program "shen-scheme.scm")

  The call to `build` generates Scheme code files in "compiled/*.scm",
  "shen-scheme-runtime.ss", and a file containing a program or public R6RS
  library definition.  The public library exports `initialize-shen`, which
  must be called before using the Shen environment.

*\

\\ Required for compiling newer versions with 0.18
(set shen.x.factorise-defun.*selector-handlers* [])
(set shen.x.factorise-defun.*selector-handlers-reg* [])
(trap-error
  ((foreign scm.) "(define-top-level-value kl:global/*property-vector* (make-parameter (kl:value '*property-vector*)))")
  (/. X ignore))

(define source-rules
  [{ | Rest] -> (source-rules-after-signature Rest)
  Rules -> Rules)

(define source-rules-after-signature
  [} | Rules] -> Rules
  [_ | Rest] -> (source-rules-after-signature Rest))

(define source-rule-arity
  [Arrow | _] -> 0 where (= Arrow (intern "->"))
  [Arrow | _] -> 0 where (= Arrow (intern "<-"))
  [_ | Rest] -> (+ 1 (source-rule-arity Rest)))

(define register-source-arities
  [] -> []
  [[define Name | Rules] | Rest]
  -> (do (update-lambda-table Name
                              (source-rule-arity (source-rules Rules)))
         (register-source-arities Rest))
  [_ | Rest] -> (register-source-arities Rest))

\\(load "kl/extension-factorise-defun.kl")
\\(load "src/factorize-patterns.shen")
(register-source-arities (read-file "src/compiler.shen"))
(load "src/compiler.shen")

(trap-error
  (shen.x.factorise-defun.initialise)
  (/. X ignore))
(_scm.initialize-compiler)
(set _scm.*compiling-shen-sources* true)

(set *maximum-print-sequence-size* 10000)

(set *shen-files*
      ["toplevel"
       "core"
       "sys"
       "dict"
       "sequent"
       "yacc"
       "reader"
       "prolog"
       "track"
       "load"
       "writer"
       "macros"
       "declarations"
       "types"
       "t-star"
       "init"
       "stlib"
       "extension-features"
       "extension-launcher"
       \\"extension-factorise-defun"
       \\"extension-programmable-pattern-matching"
       ])

(set *shen-scheme-files*
      ["overrides"
       "shen-scheme-extensions"
       "compiler"
       "native-source"
       "native-codegen"
       "native-modules"
       "native-app"
       "native-compiler"
       \\"factorize-patterns"
       ])

\* Required to avoid errors when processing functions with system names *\
(defcc shen.<name>
  X := (if (symbol? X)
           X
           (error "~A is not a legitimate function name.~%" X));)

(define prefix-fn
  F -> (_scm.prefix-op F))

(define for-each
  _ [] -> true
  F [X | Rest] -> (do (F X) (for-each F Rest)))

(define build.filter
  F Xs -> (filter-h F [] Xs))

(define filter-h
  _ Acc [] -> (reverse Acc)
  F Acc [X | Xs] -> (filter-h F [X | Acc] Xs) where (F X)
  F Acc [_ | Xs] -> (filter-h F Acc Xs))

\* Function overrides are defined in "overrides.shen".
   To avoid duplicate declarations, such overrides
   are registered and the original definitions are
   removed from the generated code. *\

(define register-override
  [define Name | Rules] -> (put Name overidden true))

(define load-overrides
  -> (for-each (function register-override)
               (read-file "src/overrides.shen")))

(define overidden?
  [defun Name | _] -> (trap-error (get Name overidden) (/. E false))
  _ -> false)

\* R6RS libraries require an explicit list of exported functions
   (there is no "export all"), which means the names of all
   function definitions in the Shen kernel have to be stored
   to be able to generate that list. *\

(set *functions* [])

(define register-export
  [defun Name | _] -> (set *functions*
                            [(prefix-fn Name) | (value *functions*)])
  _ -> skip)

(define sexp->string
  true -> "#t"
  false -> "#f"
  Comma -> "|,|" where (= Comma ,)
  Sym -> (symbol->string Sym) where (symbol? Sym)
  S -> (make-string "~R" (escape-string S)) where (string? S)
  [quote Exp] -> (@s "'" (sexp->string Exp))
  [Sexp | Sexps] -> (@s "(" (concat-strings (map (/. X (sexp->string X))
                                                 [Sexp | Sexps]))
                        ")")
  Sexp -> (make-string "~R" Sexp))

(define symbol->string
  S -> "|{|" where (= { S)
  S -> "|}|" where (= } S)
  S -> "|;|" where (= ; S)
  S -> (symbol->string-h (str S)))

(define symbol->string-h
  Str -> (@s "|" Str "|") where (element? "#" (explode Str))
  Str -> Str)

(define escape-string
  S -> (escape-string-h (explode S)))

(define escape-string-h
  [] -> ""
  ["\" | Cs] -> (@s "\\" (escape-string-h Cs))
  [C | Cs] -> (@s C (escape-string-h Cs)))

(define concat-strings
  [] -> ""
  [S | Ss] -> (@s S " " (concat-strings Ss)))

(define defun?
  [defun | _] -> true
  _ -> false)

(define defun-name
  [defun Name | _] -> Name)

(define defines-later?
  _ [] -> false
  Name [[defun Name | _] | _] -> true
  Name [_ | Rest] -> (defines-later? Name Rest))

(define keep-last-defuns
  [] -> []
  [Defun | Rest] -> (keep-last-defuns Rest)
      where (defines-later? (defun-name Defun) Rest)
  [Defun | Rest] -> [Defun | (keep-last-defuns Rest)])

\* R6RS libraries require that all defines show up before
   any other code. That means that all code in the Shen
   kernel that is not a function definition has to be
   kept until the end *\

(set *init-code* [
  [shen.initialise]
  [shen.x.features.initialise [cons (intern "shen/scheme") []]]
  \\[shen.x.factorise-defun.initialise]
  \\[shen.x.programmable-pattern-matching.initialise]
])

(define store-init-code
  Code -> (set *init-code*
                (append (value *init-code*) Code)))

(define compile-defun
  Defun -> (_scm.kl->scheme Defun))

(define read-file-unprocessed
  File -> (let Bytelist (read-file-as-bytelist File)
               S-exprs  (trap-error (compile (/. X (shen.<s-exprs> X)) Bytelist)
                                    (/. E (shen.reader-error (value shen.*residue*))))
            S-exprs))

(define compile-kl-file
  Prelude From To
  -> (let O (output "Compiling ~R...~%" From)
          Out (open To out)
          Kl (read-file-unprocessed From)
          Defuns (build.filter
                   (/. X (and (defun? X) (not (overidden? X))))
                   Kl)
          LastDefuns (keep-last-defuns Defuns)
          Exports (map (function register-export) LastDefuns)
          Init (store-init-code (build.filter
                                  (/. X (and (cons? X) (not (defun? X))))
                                  Kl))
          Scm (map (function compile-defun) LastDefuns)
          ScmS (map (function sexp->string) Scm)
          P (pr Prelude Out)
          F (for-each (/. S (pr (make-string "~A~%~%" S) Out) ) ScmS)
       (close Out)))

(define make-kl-code
  [define F | Rules] -> (shen.shendef->kldef F Rules)
  Code -> Code)

\* Port sources are translated without being loaded into the generated
   runtime, so derive their arity-table initialization from the defuns. *\
(define arity-registrations
  [] -> []
  [[defun Name Args _] | Rest]
  -> [[update-lambda-table Name (length Args)]
      | (arity-registrations Rest)]
  [_ | Rest] -> (arity-registrations Rest))

(define register-port-source-arities
  [] -> []
  [File | Rest]
  -> (do (register-source-arities
          (read-file (@s "src/" File ".shen")))
         (register-port-source-arities Rest)))

(define compile-shen-file
  From To -> (let Out (open To out)
                  Shen (read-file From)
                  Kl (map (function make-kl-code) Shen)
                  Code (append Kl (arity-registrations Kl))
                  F (for-each (/. S (pr (make-string "~R~%~%" S) Out) )
                              Code)
               (close Out)))

(define compile-init-code
  -> (let Out (open "compiled/shen-scheme-init.scm" out)
          Cmds (value *init-code*)
          Scm (map (function _scm.kl->scheme) Cmds)
          ScmS (map (function sexp->string) Scm)
          P (pr (shen-license) Out)
          F (for-each (/. S (pr (make-string "~A~%~%" S) Out) ) ScmS)
          StLib (pr (make-string "(kl:stlib.initialise)~%~%" S) Out)
       (close Out)))

(define build
  As Filename
  -> (do (register-port-source-arities
          (value *shen-scheme-files*))
         (compile-shen-file "src/compiler.shen" "kl/compiler.kl")
         \\(compile-shen-file "src/factorize-patterns.shen" "kl/factorize-patterns.kl")
         (compile-shen-file "src/overrides.shen" "kl/overrides.kl")
         (compile-shen-file "src/native-source.shen" "kl/native-source.kl")
         (compile-shen-file "src/native-codegen.shen" "kl/native-codegen.kl")
         (compile-shen-file "src/native-modules.shen" "kl/native-modules.kl")
         (compile-shen-file "src/native-app.shen" "kl/native-app.kl")
         (compile-shen-file "src/native-compiler.shen" "kl/native-compiler.kl")
         (compile-shen-file "src/shen-scheme-extensions.shen" "kl/shen-scheme-extensions.kl")
         (for-each (/. F (compile-kl-file
                          (shen-scheme-license)
                          (@s "kl/" F ".kl")
                          (@s "compiled/" F ".scm")))
                   (value *shen-scheme-files*))
         (load-overrides)
         (for-each (/. F (compile-kl-file
                          (shen-license)
                          (@s "kl/" F ".kl")
                          (@s "compiled/" F ".scm")))
                   (value *shen-files*))
         (compile-init-code)
         (compile-shen-as As Filename)
         done))

(define globals-definitions
  [] -> "c#10;"
  [Name | Rest] -> (@s "(define " (str (_scm.prefix-global Name))
                       " (make-parameter #f))c#10;"
                       (globals-definitions Rest)))

(define globals-register
  [] -> ""
  [Name | Rest] -> (@s "  (shen-global-parameter-set! '" (str Name)
                       " kl:global/" (str Name) ")c#10;"
                       (globals-register Rest)))

(define runtime-primitive-exports
  -> [register-globals
      (prefix-fn _scm.assert-boolean)
      (prefix-fn intern)
      (prefix-fn str)
      (prefix-fn set)
      (prefix-fn value)
      (prefix-fn error-to-string)
      (prefix-fn =)
      (prefix-fn eval-kl)
      (prefix-fn open)
      (prefix-fn close)
      (prefix-fn write-byte)
      (prefix-fn read-byte)
      (prefix-fn get-time)
      non-rational-/
      value/or
      get/or
      <-address/or
      <-vector/or
      make-equal-hashtable
      hashtable-fold
      read-file-as-bytelist
      read-file-as-string
      shen-scheme-native-load-init?
      error-location
      analyse-symbol?])

(define runtime-global-exports
  -> (map (function _scm.prefix-global) (value _scm.*static-globals*)))

(define runtime-exports
  Names -> (append (runtime-primitive-exports)
                   (runtime-global-exports)
                   Names))

(set *runtime-library-file* "shen-scheme-runtime.ss")

(define runtime-library-definition
  Names -> (@s
";; Copyright (c) 2012-2021 Bruno Deferrari.  All rights reserved.
;; BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause


(library (shen-scheme runtime)
  "

(sexp->string [export | (runtime-exports Names)])

"
  (import (chezscheme))

"

(globals-definitions (value _scm.*static-globals*))

"(define (register-globals)
"

(globals-register (value _scm.*static-globals*))

")

(define get-shen-scheme-home-path
  (let ((proc #f))
    (lambda ()
      (unless proc
        (set! proc
          (foreign-procedure c#34;get_shen_scheme_home_pathc#34; () string)))
      (proc))))

(include c#34;src/chez-prelude.scmc#34;)
(include c#34;src/primitives.scmc#34;)

(include c#34;compiled/overrides.scmc#34;)
(include c#34;compiled/shen-scheme-extensions.scmc#34;)

(include c#34;compiled/compiler.scmc#34;)
(include c#34;compiled/native-source.scmc#34;)
(include c#34;compiled/native-codegen.scmc#34;)
(include c#34;compiled/native-modules.scmc#34;)
(include c#34;compiled/native-app.scmc#34;)
(include c#34;compiled/native-compiler.scmc#34;)
;(include c#34;compiled/factorize-patterns.scmc#34;)
(include c#34;compiled/toplevel.scmc#34;)
(include c#34;compiled/core.scmc#34;)
(include c#34;compiled/sys.scmc#34;)
(include c#34;compiled/dict.scmc#34;)
(include c#34;compiled/sequent.scmc#34;)
(include c#34;compiled/yacc.scmc#34;)
(include c#34;compiled/reader.scmc#34;)
(include c#34;compiled/prolog.scmc#34;)
(include c#34;compiled/track.scmc#34;)
(include c#34;compiled/load.scmc#34;)
(include c#34;compiled/writer.scmc#34;)
(include c#34;compiled/macros.scmc#34;)
(include c#34;compiled/declarations.scmc#34;)
(include c#34;compiled/types.scmc#34;)
(include c#34;compiled/t-star.scmc#34;)
(include c#34;compiled/init.scmc#34;)
(include c#34;compiled/stlib.scmc#34;)
(include c#34;compiled/extension-features.scmc#34;)
(include c#34;compiled/extension-launcher.scmc#34;)
;; (include c#34;compiled/extension-factorise-defun.scmc#34;)
;; (include c#34;compiled/extension-programmable-pattern-matching.scmc#34;)

)
"))

(define loader-body
  -> "
(import (chezscheme))
(import (shen-scheme runtime))

(define initialize-shen
  (let ((initialized #f))
    (lambda ()
      (if (not initialized)
          (begin
            (include c#34;src/version.scmc#34;)
            (include c#34;src/init.scmc#34;)
            (include c#34;compiled/shen-scheme-init.scmc#34;)
            (set! initialized #t))))))
")

(define library-compatibility-body
  -> "
(define kl:shen.quiet-load kl:shen.x.launcher.quiet-load)
(define kl:shen.run-shen kl:shen-scheme.run-shen)
")

(define write-string-to-file
  Body File -> (let Out (open File out)
                    P (pr Body Out)
                 (close Out)))

(define compile-shen-as
  library Filename -> (do
                       (write-string-to-file
                        (runtime-library-definition (value *functions*))
                        (value *runtime-library-file*))
                       (write-string-to-file
                        (library-definition (value *functions*))
                        Filename))
  program Filename -> (do
                       (write-string-to-file
                        (runtime-library-definition (value *functions*))
                        (value *runtime-library-file*))
                       (write-string-to-file
                        (program-definition)
                        Filename)))

(define initialization-body
  -> "(suppress-greeting #t)

(scheme-start
  (lambda fns
    (initialize-shen)
    (kl:shen-scheme.run-shen fns)
    (exit 0)))")

(define program-definition
  -> (make-string "~A~%~A~%~A~%"
                  (shen-scheme-license)
                  (loader-body)
                  (initialization-body)))

(define library-definition
  Names -> (let Exports [export initialize-shen
                                (prefix-fn _scm.assert-boolean)
                                (prefix-fn intern)
                                (prefix-fn str)
                                (prefix-fn set)
                                (prefix-fn value)
                                (prefix-fn error-to-string)
                                (prefix-fn =)
                                (prefix-fn eval-kl)
                                (prefix-fn open)
                                (prefix-fn close)
                                (prefix-fn write-byte)
                                (prefix-fn read-byte)
                                (prefix-fn get-time)
                                (prefix-fn shen.quiet-load)
                                (prefix-fn shen.run-shen)
                                | Names]
             (make-string "~A~%(library (shen)~%  ~A~%  ~A~%~A)"
                          (shen-scheme-license)
                          (sexp->string Exports)
                          (loader-body)
                          (library-compatibility-body))))

(define shen-license
  -> ";; Copyright (c) 2015, Mark Tarver
;;
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of Mark Tarver may not be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY Mark Tarver ''AS IS'' AND ANY
;; EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL Mark Tarver BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

")

(define shen-scheme-license
  -> ";; Copyright (c) 2012-2021 Bruno Deferrari.  All rights reserved.
;; BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

")
