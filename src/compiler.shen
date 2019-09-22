\* Copyright (c) 2012-2019 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(package _scm [begin quote string->symbol null? car cdr pair? else
                     vector-ref vector-set! make-vector string non-rational-/
                     string-append integer->char char->integer
                     string-ref string-length substring
                     eq? equal? scm. scm.import import *toplevel*
                     letrec scm.letrec scm.with-input-from-string scm.read
                     scm.define scm.goto-label scm.begin
                     scm.value/or scm.get/or scm.<-vector/or scm.<-address/or]

(define initialize-compiler
  -> (do (set *compiling-shen-sources* false)
         (set *yields-boolean2* [or and < > >= <= =])
         (set *yields-boolean1*
               [not
                string? vector? number? cons? absvector? element? symbol?
                tuple? variable? boolean? empty? shen.pvar?])
         (set *kl-prefix* (intern "kl:"))))

(define unbound-symbol?
  Sym Scope -> (not (element? Sym Scope)) where (or (symbol? Sym) (= Sym ,))
  _ _ -> false)

\* Used to keep track of the function being compiled for error messages *\
(set *compiling-function* [*toplevel*])

(define compile-expression
  [] _ -> [quote []]
  Sym Scope -> (emit-symbol Sym) where (unbound-symbol? Sym Scope)
  [let Var Value Body] Scope -> (emit-let Var Value Body Scope)
  [cond | Clauses] Scope -> (emit-cond Clauses Scope)
  [if Test Then Else] Scope -> (emit-if Test Then Else Scope)
  [lambda Var Body] Scope -> [lambda [Var]
                               (compile-expression Body [Var | Scope])]
  [and E1 E2] Scope -> [and
                        (compile-expression (force-boolean E1) Scope)
                        (compile-expression (force-boolean E2) Scope)]
  [or E1 E2] Scope -> [or
                       (compile-expression (force-boolean E1) Scope)
                       (compile-expression (force-boolean E2) Scope)]
  [trap-error Exp Handler] Scope -> (emit-trap-error Exp Handler Scope)
  [do E1 E2] Scope -> [begin (compile-expression E1 Scope)
                             (compile-expression E2 Scope)]
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
  [scm.define [Name | Vars] Body] Scope -> [define [Name | Vars] (compile-expression Body (append Vars Scope))]
  [scm.begin | Exprs] Scope -> [begin | (map (/. Exp (compile-expression Exp Scope)) Exprs)]
  [scm.goto-label F | Args] _ -> [F | Args]
  [scm. Code] _ -> (if (string? Code)
                       (scm.with-input-from-string Code (freeze (scm.read)))
                       (error "scm. excepts a string, not ~A" Code))
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
  \* TODO: optimize integers with `eq?` too *\
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

(define partial-application?
  Op Arity Params -> (not (or (= Arity -1)
                              (= Arity (length Params)))))

(define take
  _ 0 -> []
  [X | Xs] N -> [X | (take Xs (- N 1))])

(define drop
  Xs 0 -> Xs
  [X | Xs] N -> (drop Xs (- N 1)))

\* TODO: optimize cases where the args are static values *\
(define emit-partial-application
  Op Arity Params Scope
  -> (let Args (map (/. P (compile-expression P Scope)) Params)
       (nest-call (nest-lambda Op Arity Scope) Args))
    where (> Arity (length Params))
  Op Arity Params Scope
  -> (let App (compile-expression [Op | (take Params Arity)] Scope)
          Rest (map (/. X (compile-expression X Scope)) (drop Params Arity))
       (nest-call App Rest))
    where (< Arity (length Params))
  _ _ _ _ -> (error "emit-partial-application called with non-partial application"))

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
  (@s "scm." Rest) -> (intern Rest))

(define prefix-op
  Sym -> (remove-scm-prefix Sym) where (scm-prefixed? Sym)
  Sym -> (concat (value *kl-prefix*) Sym) where (symbol? Sym)
  NotSym -> NotSym)

(define not-fail
  Obj F -> (F Obj) where (not (= Obj (fail)))
  Obj _ -> Obj)

(define emit-static-application
  Op 2 Params Scope <- (not-fail
                        (binary-op-mapping Op)
                        (/. MappedOp
                            (let Args (map (/. P (compile-expression P Scope))
                                           Params)
                              [MappedOp | Args])))
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
      \* Known function without all arguments *\
      (partial-application? Op Arity Params)
      (emit-partial-application Op Arity Params Scope)
      \* Variables or results of expressions *\
      (dynamic-application? Op Scope)
      (emit-dynamic-application Op Params Scope)
      \* Known function with all arguments *\
      true
      (emit-static-application Op Arity Params Scope)))

(define nest-call
  Op [] -> Op
  Op [Arg | Args] -> (nest-call [Op Arg] Args))

(define nest-lambda
  Callable Arity Scope
  -> (compile-expression Callable Scope)
     where (<= Arity 0)

  Callable Arity Scope
  -> (let ArgName (gensym (protect Y))
       [lambda [ArgName]
         (nest-lambda (merge-args Callable ArgName)
                      (- Arity 1)
                      [ArgName | Scope])]))

(define merge-args
  Op Arg -> (append Op [Arg]) where (cons? Op)
  Op Arg -> [Op Arg])

(define compiling-function
  Name F -> (let _ (set *compiling-function* [Name | (value *compiling-function*)])
                 Result (thaw F)
                 _ (set *compiling-function* (tl (value *compiling-function*)))
              Result))

(define kl->scheme
  [defun Name Args [cond | Cases]] -> (kl->scheme
                                       (compiling-function Name
                                        (freeze
                                         (factorize-defun
                                           [defun Name Args [cond | Cases]]))))
  [defun Name Args Body] -> (compiling-function Name
                              (freeze [define [(prefix-op Name) | Args]
                                        (compile-expression Body Args)]))
  Exp -> (compile-expression Exp []))

)
