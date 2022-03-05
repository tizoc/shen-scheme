\* Copyright (c) 2012-2021 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

\* How to use:

    (load "build.shen")
    (build program "shen-scheme.scm")

  The call to `build` will generate Scheme code files in "compiled/*.scm",
  and a "shen-scheme.scm" file containing a program or R6RS library definition.
  All initialization code will be contained in a `shen-initialize`
  function that has to be called to initialize the Shen environment
  before using it.

*\

\\ Required for compiling newer versions with 0.18
(set shen.x.factorise-defun.*selector-handlers* [])
(set shen.x.factorise-defun.*selector-handlers-reg* [])
(trap-error
  (scm. "(define-top-level-value kl:global/*property-vector* (make-parameter (kl:value '*property-vector*)))")
  (/. X ignore))

\\(load "kl/extension-factorise-defun.kl")
\\(load "src/factorize-patterns.shen")
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
       "extension-features"
       "extension-launcher"
       \\"extension-factorise-defun"
       \\"extension-programmable-pattern-matching"
       ])

(set *shen-scheme-files*
      ["overrides"
       "shen-scheme-extensions"
       "compiler"
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

(define filter
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
  [quote Exp] -> (@s "'" (sexp->string Exp))
  [Sexp | Sexps] -> (@s "(" (concat-strings (map (/. X (sexp->string X))
                                                 [Sexp | Sexps]))
                        ")")
  Sexp -> (make-string "~R" Sexp))

(define symbol->string
  S -> "|{|" where (= { S)
  S -> "|}|" where (= } S)
  S -> "|;|" where (= ; S)
  S -> (str S))

(define concat-strings
  [] -> ""
  [S | Ss] -> (@s S " " (concat-strings Ss)))

(define defun?
  [defun | _] -> true
  _ -> false)

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

(define compile-kl-file
  Prelude From To
  -> (let O (output "Compiling ~R...~%" From)
          Out (open To out)
          Kl (read-file From)
          Defuns (filter (/. X (and (defun? X)
                                    (not (overidden? X))))
                         Kl)
          Exports (map (function register-export) Defuns)
          Init (store-init-code (filter (/. X (and (cons? X)
                                                   (not (defun? X))))
                                        Kl))
          Scm (map (function compile-defun) Defuns)
          ScmS (map (function sexp->string) Scm)
          P (pr Prelude Out)
          F (for-each (/. S (pr (make-string "~A~%~%" S) Out) ) ScmS)
       (close Out)))

(define make-kl-code
  [define F | Rules] -> (shen.shendef->kldef F Rules)
  Code -> Code)

(define compile-shen-file
  From To -> (let Out (open To out)
                  Shen (read-file From)
                  Kl (map (function make-kl-code) Shen)
                  F (for-each (/. S (pr (make-string "~R~%~%" S) Out) )
                              Kl)
               (close Out)))

(define compile-init-code
  -> (let Out (open "compiled/shen-scheme-init.scm" out)
          Cmds (value *init-code*)
          Scm (map (function _scm.kl->scheme) Cmds)
          ScmS (map (function sexp->string) Scm)
          P (pr (shen-license) Out)
          F (for-each (/. S (pr (make-string "~A~%~%" S) Out) ) ScmS)
       (close Out)))

(define build
  As Filename
  -> (do (compile-shen-file "src/compiler.shen" "kl/compiler.kl")
         \\(compile-shen-file "src/factorize-patterns.shen" "kl/factorize-patterns.kl")
         (compile-shen-file "src/overrides.shen" "kl/overrides.kl")
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

(define loader-body
  -> (@s
"(import (chezscheme))

"

(globals-definitions (value _scm.*static-globals*))

"(define (register-globals)
"

(globals-register (value _scm.*static-globals*))

")

(include c#34;src/chez-prelude.scmc#34;)
(include c#34;src/primitives.scmc#34;)

(include c#34;compiled/overrides.scmc#34;)
(include c#34;compiled/shen-scheme-extensions.scmc#34;)

(include c#34;compiled/compiler.scmc#34;)
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
(include c#34;compiled/extension-features.scmc#34;)
(include c#34;compiled/extension-launcher.scmc#34;)
;; (include c#34;compiled/extension-factorise-defun.scmc#34;)
;; (include c#34;compiled/extension-programmable-pattern-matching.scmc#34;)

(define initialize-shen
  (let ((initialized #f))
    (lambda ()
      (if (not initialized)
          (begin
            (define-top-level-value 'get-shen-scheme-home-path (foreign-procedure c#34;get_shen_scheme_home_pathc#34; () string))
            (include c#34;src/version.scmc#34;)
            (include c#34;src/init.scmc#34;)
            (include c#34;compiled/shen-scheme-init.scmc#34;)
            (set! initialized #t))))))
"))

(define write-string-to-file
  Body File -> (let Out (open File out)
                    P (pr Body Out)
                 (close Out)))

(define compile-shen-as
  library Filename -> (write-string-to-file
                       (library-definition (value *functions*))
                       Filename)
  program Filename -> (write-string-to-file
                       (program-definition)
                       Filename))

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

\* library-definition is unused for now *\

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
             (make-string "~A~%(library (shen)~%  ~R~%  ~A)"
                          (shen-scheme-license)
                          Exports
                          (loader-body))))

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
