\* Copyright (c) 2012-2021 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(package _scm [begin quote string->symbol null? car cdr pair? else
                     vector-ref vector-set! make-vector string non-rational-/
                     string-append integer->char char->integer
                     string-ref string-length substring list kl
                     eq? eqv? equal? scm. scm.import import *sterror* *toplevel*
                     letrec let* scm.letrec scm.with-input-from-string scm.read
                     scm.define scm.goto-label scm.begin
                     scm.value/or scm.get/or scm.<-vector/or scm.<-address/or]

(define initialize-compiler
  -> (do (set *compiling-shen-sources* false)
         (set *yields-boolean2* [or and < > >= <= =])
         (set *yields-boolean1*
               [not
                string? vector? number? cons? absvector? element? symbol?
                tuple? variable? boolean? empty? shen.pvar?
                shen.+string? shen.+vector?])
         (set *kl-prefix* (intern "kl:"))
         (set *static-globals* [
           shen.*infs* shen.*call* shen.*occurs*
           shen.*special* shen.*extraspecial*
           shen.*platform-native-call-check*
           shen.*demodulation-function*
           shen.*gensym*
           *stinput* *stoutput* *sterror*
           *property-vector* *macros*
           _scm.*kl-prefix*
           _scm.*global-prefix*
           _scm.*yields-boolean1*
           _scm.*yields-boolean2*
           \\ TODO
           \\*maximum-print-sequence-size*
           \\_scm.*static-globals*
           \\_scm.*compiling-shen-sources*
         ])
         (set *global-prefix* (intern "kl:global/"))))

(define unbound-symbol?
  Sym Scope -> (not (element? Sym Scope)) where (or (symbol? Sym) (= Sym ,))
  _ _ -> false)

\* Used to keep track of the function being compiled for error messages *\
(set *compiling-function* [*toplevel*])

(define merge-nested-repeats
  Op [Op Exp1 [Op | Exps]] -> [Op Exp1 | Exps]
  _ X -> X)

(define merge-nested-lets
  [let [Binding] [let* Bindings Body]] -> [let* [Binding | Bindings] Body]
  [let [Binding1] [let [Binding2] Body]] -> [let* [Binding1 Binding2] Body]
  X -> X)

(define static-global?
  Var -> (trap-error (element? Var (value *static-globals*))
                     (/. E false)))

(define compile-expression
  [] _ -> [quote []]
  Sym Scope -> (emit-symbol Sym) where (unbound-symbol? Sym Scope)
  [set Var Value] Scope -> [let [[tmp (compile-expression Value Scope)]]
                              [(prefix-global Var) tmp]
                              tmp]
      where (static-global? Var)
  [value Var] _ -> [(prefix-global Var)]
      where (static-global? Var)
  [let Var Value Body] Scope -> (merge-nested-lets
                                 (emit-let Var Value Body Scope))
  [cond | Clauses] Scope -> (emit-cond Clauses Scope)
  [if Test Then Else] Scope -> (emit-if Test Then Else Scope)
  [lambda Var Body] Scope -> [lambda [Var]
                               (compile-expression Body [Var | Scope])]
  [and E1 E2] Scope -> (merge-nested-repeats and
                         [and
                          (compile-expression (force-boolean E1) Scope)
                          (compile-expression (force-boolean E2) Scope)])
  [or E1 E2] Scope -> (merge-nested-repeats or
                        [or
                         (compile-expression (force-boolean E1) Scope)
                         (compile-expression (force-boolean E2) Scope)])
  [trap-error Exp Handler] Scope -> (emit-trap-error Exp Handler Scope)
  [do E1 E2] Scope -> (merge-nested-repeats begin
                        [begin (compile-expression E1 Scope)
                               (compile-expression E2 Scope)])
  [freeze Exp] Scope -> [lambda [] (compile-expression Exp Scope)]
  [thaw Exp] Scope -> [(compile-expression Exp Scope)]
  [= A B] Scope -> (emit-equality-check A B Scope)
  [intern S] _ -> [quote (intern S)] where (string? S)
  [type Exp _] Scope -> (compile-expression Exp Scope)
  [simple-error Msg] Scope -> [error [quote (hd (value *compiling-function*))]
                                     (compile-expression Msg Scope)]
  [n->string N] Scope -> [string [integer->char (compile-expression N Scope)]]
  [string->n S] Scope -> [char->integer [string-ref (compile-expression S Scope) 0]]
  [pos S N] Scope -> [string [string-ref (compile-expression S Scope)
                                         (compile-expression N Scope)]]
  [tlstr S] Scope -> [let [[tmp (compile-expression S Scope)]]
                       [substring tmp 1 [string-length tmp]]]
  [absvector N] Scope -> [make-vector (compile-expression N Scope)
                                      [(prefix-op fail)]]
  [<-address V N] Scope -> [vector-ref (compile-expression V Scope)
                                       (compile-expression N Scope)]
  [address-> V N X] Scope -> [let [[tmp (compile-expression V Scope)]]
                               [vector-set! tmp
                                            (compile-expression N Scope)
                                            (compile-expression X Scope)]
                               tmp]
  [scm.import | Rest] _ -> [import | Rest]
  [scm.letrec | Rest] Scope -> (emit-letrec Rest Scope)
  [scm.lambda Vars Body] Scope -> [lambda Vars (compile-expression Body (append Vars Scope))]
  [scm.define [Name | Vars] Body] Scope -> [define [Name | Vars] (compile-expression Body (append Vars Scope))]
  [scm.define Name Expr] Scope -> [define Name (compile-expression Expr Scope)] where (symbol? Name)
  [scm. Code] _ -> ((foreign scm.with-input-from-string) Code (freeze ((foreign scm.read)))) where (string? Code)
  [scm. Form] _ -> (emit-scm-form Form)
  [scm. kl : Name] _ -> (intern (cn "kl:" (str Name))) where (symbol? Name)
  [Op | Args] Scope -> (emit-application Op Args Scope)
  X _ -> X                      \* literal *\
  )

(define yields-boolean?
  true -> true
  false -> true
  [let _ _ Exp] -> (yields-boolean? Exp)
  [do _ Exp] -> (yields-boolean? Exp)
  [X _ _] -> (element? X (value *yields-boolean2*))
  [X _] -> (element? X (value *yields-boolean1*))
  _ -> false)

(define force-boolean
  X -> X where (yields-boolean? X)
  X -> [assert-boolean X])

(define emit-symbol
  S -> [quote S])

(define emit-let
  Var Value Body Scope
  -> [let [[Var (compile-expression Value Scope)]]
       (compile-expression Body [Var | Scope])])

(define emit-scm-form
  Form -> (without-scm-prefixes Form))

(define without-scm-prefixes
  [] -> []
  [H | T] -> [(without-scm-prefixes H) | (without-scm-prefixes T)]
  Sym -> (remove-scm-prefix Sym) where (scm-prefixed? Sym)
  Other -> Other)

(define valid-letrec-bindings?
  [] -> true
  [[Var Value] | Rest] -> (valid-letrec-bindings? Rest) where (variable? Var)
  _ -> false)

(define emit-letrec
  [Bindings | Body] Scope
  -> (let NewVars (map (/. Pair (hd Pair)) Bindings)
          NewScope (append NewVars Scope)
          LetBindings (emit-letrec-bindings Bindings NewScope)
          CompiledBody (map (/. Code (compile-expression Code NewScope)) Body)
       [letrec LetBindings | CompiledBody])
    where (and (not (= Body []))
               (valid-letrec-bindings? Bindings))
  X _ -> (error "Invalid letrec bindings structure ~A" X))

(define emit-letrec-bindings
  [] _ -> []
  [[Var Value] | Rest] Scope -> [[Var (compile-expression Value Scope)] |
                                 (emit-letrec-bindings Rest Scope)])

(define emit-if
  true Then _ Scope  -> (compile-expression Then Scope)
  false _ Else Scope -> (compile-expression Else Scope)
  Test Then Else Scope
  -> [if (compile-expression (force-boolean Test) Scope)
         (compile-expression Then Scope)
         (compile-expression Else Scope)])

(define emit-cond
  Clauses Scope -> [cond | (emit-cond-clauses Clauses Scope)])

(define emit-cond-clauses
  [] _ -> []
  [[Test Body] | Rest] Scope
  -> (let CompiledTest (compile-expression (force-boolean Test) Scope)
          CompiledBody (compile-expression Body Scope)
          CompiledRest (emit-cond-clauses Rest Scope)
       [[CompiledTest CompiledBody]
        | CompiledRest]))

(define emit-trap-error
  [F | Rest] Handler Scope <- (emit-trap-error-optimize [F | Rest] Handler Scope)
      where (and (value *compiling-shen-sources*)
                 (element? F [value <-vector <-address get]))

  Exp [lambda E Handler] Scope
  -> [(intern "guard") [E [else (compile-expression Handler [E | Scope])]]
       (compile-expression Exp Scope)]

  Exp Handler Scope
  -> [let [[(intern "?handler") (compile-expression Handler Scope)]]
       [(intern "guard") [(intern "?exn") [else [(intern "?handler")
                                                 (intern "?exn")]]]
        (compile-expression Exp Scope)]])

\*
NOTE: This transformation assumes that:
- the operand expressions will not raise their own exception,
- the operands are of the right type,
- the Handler doesn't make use of the error
otherwise the result is not semantically equivalent to the original code.

For this reason it is only enabled when compiling the Shen Kernel sources
but not otherwise.
*\
(define emit-trap-error-optimize
  [value X] [lambda E Handler] Scope
  -> (compile-expression [scm.value/or X [freeze Handler]] Scope)
  [<-vector X N] [lambda E Handler] Scope
  -> (compile-expression [scm.<-vector/or X N [freeze Handler]] Scope)
  [<-address X N] [lambda E Handler] Scope
  -> (compile-expression [scm.<-address/or X N [freeze Handler]] Scope)
  [get X P D] [lambda E Handler] Scope
  -> (compile-expression [scm.get/or X P D [freeze Handler]] Scope)
  _ _ _ -> (fail))

(define emit-equality-check
  V1 V2 Scope -> [eq? (compile-expression V1 Scope)
                      (compile-expression V2 Scope)]
      where (or (unbound-symbol? V1 Scope)
                (unbound-symbol? V2 Scope)
                (= [fail] V1)
                (= [fail] V2))
  V1 V2 Scope -> [eqv? (compile-expression V1 Scope)
                       (compile-expression V2 Scope)]
      where (or (number? V1) (number? V2))
  V1 V2 Scope -> [equal? (compile-expression V1 Scope)
                         (compile-expression V2 Scope)]
      where (or (string? V1) (string? V2))
  [] V2 Scope -> [null? (compile-expression V2 Scope)]
  V1 [] Scope -> [null? (compile-expression V1 Scope)]
  V1 V2 Scope -> [(intern "kl:=")
                  (compile-expression V1 Scope)
                  (compile-expression V2 Scope)])

(define binary-op-mapping
  +               -> +
  -               -> -
  *               -> *
  /               -> non-rational-/
  >               -> >
  <               -> <
  >=              -> >=
  <=              -> <=
  cons            -> cons
  cn              -> string-append
  _               -> (fail))

(define unary-op-mapping
  number?         -> number?
  string?         -> string?
  cons?           -> pair?
  absvector?      -> vector?
  hd              -> car
  tl              -> cdr
  _               -> (fail))

(define emit-application
  Op Params Scope -> (emit-application* Op (arity Op) Params Scope))

(define dynamic-application?
  Op Scope -> (or (cons? Op) (element? Op Scope)))

(define emit-dynamic-application
  Op [] Scope -> [(compile-expression Op Scope)] \* empty case *\
  Op Params Scope
  -> (let Args (map (/. P (compile-expression P Scope)) Params)
       (nest-call (compile-expression Op Scope)
                  Args)))

(define scm-prefixed-h?
  [($ scm.) | _] -> true
  _ -> false)

(define scm-prefixed?
  Sym -> (scm-prefixed-h? (explode Sym)) where (symbol? Sym)
  _ -> false)

(define remove-scm-prefix
  Sym -> (remove-scm-prefix (str Sym)) where (symbol? Sym)
  "scm." -> scm.
  (@s "scm." Rest) -> (intern Rest))

(define prefix-op
  Sym -> (remove-scm-prefix Sym) where (scm-prefixed? Sym)
  Sym -> (concat (value *kl-prefix*) Sym) where (symbol? Sym)
  NotSym -> NotSym)

(define prefix-global
  Sym -> (concat (value *global-prefix*) Sym))

(define not-fail
  Obj F -> (F Obj) where (not (= Obj (fail)))
  Obj _ -> Obj)

(define listify-conses
  [cons Exp [quote []]] -> [list Exp]
  [cons Exp [list | List]] -> [list Exp | List]
  Exp -> Exp)

(define emit-static-application
  Op 2 Params Scope <- (not-fail
                        (binary-op-mapping Op)
                        (/. MappedOp
                            (let Args (map (/. P (compile-expression P Scope))
                                           Params)
                              (listify-conses
                               [MappedOp | Args]))))
  Op 1 Params Scope <- (not-fail
                        (unary-op-mapping Op)
                        (/. MappedOp
                            (let Args (map (/. P (compile-expression P Scope))
                                           Params)
                              [MappedOp | Args])))
  Op _ Params Scope -> (let Args (map (/. P (compile-expression P Scope))
                                      Params)
                         [(prefix-op Op) | Args]))

(define emit-application*
  Op Arity Params Scope
  -> (cases
      \* Variables or results of expressions *\
      (dynamic-application? Op Scope)
      (emit-dynamic-application Op Params Scope)
      \* Known function with all arguments *\
      true
      (emit-static-application Op Arity Params Scope)))

(define nest-call
  Op [] -> Op
  Op [Arg | Args] -> (nest-call [Op Arg] Args))

(define compiling-function
  Name F -> (let S (set *compiling-function* [Name | (value *compiling-function*)])
                 Result (thaw F)
                 S (set *compiling-function* (tl (value *compiling-function*)))
              Result))

(define kl->scheme
  [defun Name Args Body] -> (compiling-function Name
                              (freeze [define [(prefix-op Name) | Args]
                                        (compile-expression Body Args)]))
  Exp -> (compile-expression Exp []))

)
