\* Copyright (c) 2012-2019 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(package _scm [scm.goto-label scm.begin scm.define $label *toplevel*]

\*

# Pattern factorization optimization

## The problem

The way Shen compiles pattern matching code to Klambda can result
in too many repeated tests and selectors, which is suboptimal.
Since there isn't a reliable way for Shen to solve this portably,
the task is deferred to the port.

The first step is to process the `cond` construct at the root
of the function definition, and rebuilt it into an if-else
tree, with all the duplicated tests merged into a single instance.
But this results in code duplication, because this restructuring
of the code forces the bodies of the false branches to be
duplicated.

The solution to this is to remove the duplication by binding
a continuation using this code, and to generate a jump to this
continuation in the false branches. This requires that the underlying
platform supports this efficiently through direct jumps, like for
example with GOTO.

The other part of the optimization involves finding duplicate
instances of selectors, binding the result of the selector
to a variable on the outer scope, and replacing all instances
of such selector with a reference to this variable.

## Chez specifics

Chez doesn't provide an efficient GOTO-like construct, but
for functions defined inside of the function being processed,
if no references are made to variables in the outer scope, Chez
is able to compile calls to these functions into very efficient
jumps.

To be able to do this, after all the continuation bodies
have been associated with a label, a second pass is done,
on which the bodies of the labels are lifted into functions
at the root of the function being optimized, and also scanned
for references to free variables which are then added as
parameters to the lifted function. All invocations to the
label are also updated to include the necessary arguments.

The entry point is `factorize-defun`, the inputs is a `defun` declaration.
Rebuilding of the `cond` construct is handled by the `rebranch` function.
Selector optimization by the `optimize-selectors` function.
Label hoisting is the second pass, handled by `hoist-labels`. It converts
labels into functions that get hoisted to the root of the `defun` and
updates all the label invocations accordingly.

*\

(define factorize-defun
  [defun F Params [cond | Cases]]
  -> (let PreBody (rebranch Cases Params [shen.f-error F])
          Body+Continuations (hoist-labels PreBody Params [])
          Body (fst Body+Continuations)
          Continuations (reverse (snd Body+Continuations))
       [defun F Params [scm.begin | (append Continuations [Body])]])
  X -> X)

(define free-variables-h
  [let Var Value Body] Scope Acc -> (free-variables-h Body (remove Var Scope)
                                      (free-variables-h Value Scope Acc))
  [lambda Var Body] Scope Acc -> (free-variables-h Body (remove Var Scope) Acc)
  [scm.goto-label Label | Args] Scope Acc -> (free-variables-h Args Scope Acc)
  [X | Xs] Scope Acc -> (free-variables-h Xs Scope
                          (free-variables-h X Scope Acc))
  Var Scope Acc -> (adjoin Var Acc)
      where (element? Var Scope)
  _ _ Acc -> Acc)

(define free-variables
  Body Scope -> (reverse (free-variables-h Body Scope [])))

\\ NOTE: a label is only hoisted if it is invoked more than
\\ once, otherwise its body is inlined at the single place
\\ where it is invoked.
(define hoist-labels
  [let-label Label LabelBody Body] Scope Acc
  -> (let NewLabelBody+NewAcc (hoist-labels LabelBody Scope Acc)
          Vars (free-variables LabelBody Scope)
          NewBody (subst [scm.goto-label Label | Vars] [scm.goto-label Label] Body)
          Continuation [scm.define [Label | Vars] (fst NewLabelBody+NewAcc)]
          NewAcc [Continuation | (snd NewLabelBody+NewAcc)]
       (hoist-labels NewBody Scope NewAcc))
      where (> (occurrences [scm.goto-label Label] Body) 1)

  [let-label Label LabelBody Body] Scope Acc
  -> (let NewLabelBody+NewAcc (hoist-labels LabelBody Scope Acc)
          NewAcc (snd NewLabelBody+NewAcc)
          NewBody (subst (fst NewLabelBody+NewAcc) [scm.goto-label Label] Body)
       (hoist-labels NewBody Scope NewAcc))

  [if Test Then Else] Scope Acc
  -> (let NewThen+NewAcc (hoist-labels Then Scope Acc)
       (@p [if Test (fst NewThen+NewAcc) Else] (snd NewThen+NewAcc)))

  [let Var Val Body] Scope Acc
  -> (let NewBody+NewAcc (hoist-labels Body [Var | Scope] Acc)
       (@p [let Var Val (fst NewBody+NewAcc)] (snd NewBody+NewAcc)))

  Body _ Acc -> (@p Body Acc))

(define generate-label
  -> (let FName (hd (value *compiling-function*))
       (if (= *toplevel* FName)
           (gensym $label)
           (gensym (concat FName $label)))))

(define with-labelled-else
  Atom F -> (F Atom) where (not (cons? Atom))
  [scm.goto-label Label] F -> (F [scm.goto-label Label])
  Body F -> (let Label (generate-label)
              [let-label Label Body
                (F [scm.goto-label Label])]))

\\ When an immediate child if-branch has the same
\\ Else as the parent, merge into a single if with (and T1 T2)
(define merge-same-else-ifs
  [if Test1 [if Test2 Then2 Else] Else] -> [if [and Test1 Test2] Then2 Else]
  X -> X)

(define rebranch-h
  Test Scope TrueBranch FalseBranch Else
  -> (let NewElse (rebranch FalseBranch Scope Else)
       (with-labelled-else NewElse
        (/. GotoElse
         (merge-same-else-ifs
          [if Test
              (optimize-selectors Test (rebranch TrueBranch Scope GotoElse))
              GotoElse])))))

(define rebranch
  [] _ Else -> Else
  [[true Result] | _] _ _ -> Result
  [[[and Test MoreTs] Result] | Cases] Scope Else
  -> (let TrueBranch (true-branch Test [[[and Test MoreTs] Result] | Cases])
          FalseBranch (false-branch Test [[[and Test MoreTs] Result] | Cases])
       (rebranch-h Test Scope TrueBranch FalseBranch Else))
  [[Test Result] | Cases] Scope Else
  -> (let TrueBranch (true-branch Test [[Test Result] | Cases])
          FalseBranch (false-branch Test [[Test Result] | Cases])
      (rebranch-h Test Scope TrueBranch FalseBranch Else)))

(define true-branch
  Test [[[and Test MoreTs] Result] | Cases]
  -> [[MoreTs Result] | (true-branch Test Cases)]
  Test [[Test Result] | Cases] -> [[true Result]]
  _ _ -> [])

(define false-branch
  Test [[[and Test MoreTs] Result] | Cases] -> (false-branch Test Cases)
  Test [[Test Result] | Cases] -> (false-branch Test Cases)
  _ Cases -> Cases)

(define concat/
  A B -> (concat A (concat / B)))

(define exp-var
  [SelF Exp] -> (concat/ (exp-var Exp) SelF)
      where (element? SelF [hd tl hdv tlv fst snd tlstr])
  [hdstr Exp] -> (concat/ (exp-var Exp) hdstr)
  Var -> Var)

(define optimize-selectors
  Test Code -> (bind-repeating-selectors (test->selectors Test) Code))

(define test->selectors
  [cons? X] -> [[hd X] [tl X]]
  [tuple? X] -> [[fst X] [snd X]]
  [string? X] -> [[hdstr X] [tlstr X]]
  [vector? X] -> [[hdv X] [tlv X]]
  _ -> [])

(define bind-repeating-selectors
  [SelA SelB] Body -> (bind-selector SelA (bind-selector SelB Body))
   _ Body -> Body)

(define bind-selector
  Sel Body -> (let Var (exp-var Sel)
                [let Var Sel (subst Var Sel Body)])
      where (> (occurrences Sel Body) 1)
  _ Body -> Body)

)