\\ Copyright (c) 2012-2019 Bruno Deferrari.  All rights reserved.
\\ BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(package _scm [%%goto-label %%let-label %%return scm.begin scm.define scm.]

\*

# Pattern factorization optimization

## Chez specifics

Chez doesn't provide an efficient GOTO-like construct, but
for functions defined inside of the function being processed,
if no references are made to variables in the outer scope, Chez
is able to compile calls to these functions into very efficient
jumps.

Using `hoist-labels`, all labels are converted into functions
declared with Scheme's `define` and moved to the root of the function.

Jumps to labels become direct function calls.

*\

(define factorize-defun
  [defun F Params [cond | Cases]]
  -> (let FactorizedDefun (shen.x.factorise-defun.factorise-defun
                           [defun F Params [cond | Cases]])
       (hoist-defun-labels FactorizedDefun))
  X -> X)

(define hoist-defun-labels
  [defun F Params LabelledBody]
  -> (let Body+Continuations (hoist-labels LabelledBody [])
          Body (fst Body+Continuations)
          Continuations (snd Body+Continuations)
       [defun F Params [scm.begin | (append Continuations [Body])]]))

(define hoist-labels
  [%%let-label [Label | Vars] LabelBody Body] Acc
  -> (let NewLabelBody+NewAcc (hoist-labels LabelBody Acc)
          NewBody (subst [(concat scm. Label) | Vars]
                         [%%goto-label Label | Vars]
                         Body)
          Continuation [scm.define [Label | Vars] (fst NewLabelBody+NewAcc)]
          NewAcc [Continuation | (snd NewLabelBody+NewAcc)]
       (hoist-labels NewBody NewAcc))

  [if Test Then Else] Acc
  -> (let NewThen+NewAcc (hoist-labels Then Acc)
          NewElse+NewAcc (hoist-labels Else (snd NewThen+NewAcc))
       (@p [if Test (fst NewThen+NewAcc) (fst NewElse+NewAcc)]
           (snd NewElse+NewAcc)))

  [let Var Val Body] Acc
  -> (let NewBody+NewAcc (hoist-labels Body Acc)
       (@p [let Var Val (fst NewBody+NewAcc)]
           (snd NewBody+NewAcc)))

  [%%return Body] Acc -> (hoist-labels Body Acc)

  Body Acc -> (@p Body (reverse Acc)))

)
