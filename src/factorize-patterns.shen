(package _scm [scm.goto-label scm.begin scm.define $label hdstr]

(define factorize-defun
  [defun F Params [cond | Cases]]
  -> (let Body (rebranch Cases Params [shen.f-error F])
          Continuations (bind-continuations (reverse (value *continuations*)))
          _ (set *continuations* [])
       [defun F Params [scm.begin | (append Continuations [Body])]])
  X -> X)

(define free-variables-h
  [let Var Value Body] Scope Acc -> (free-variables-h Body (remove Var Scope) (free-variables-h Value Scope Acc))
  [lambda Var Body] Scope Acc -> (free-variables-h Body (remove Var Scope) Acc)
  [scm.goto-label Label | Args] Scope Acc -> (free-variables-h Args Scope Acc)
  [X | Xs] Scope Acc -> (free-variables-h Xs Scope (free-variables-h X Scope Acc))
  Var Scope Acc -> (adjoin Var Acc)
      where (element? Var Scope)
  _ _ Acc -> Acc)

(define free-variables
  Body Scope -> (reverse (free-variables-h Body Scope [])))

(set *continuations* [])

(define generate-label
  -> (gensym (concat (hd (value *compiling-function*)) $label)))

(define push-continuation
  Vars Cont -> (let Label (generate-label)
                    _ (set *continuations* [[Label Vars Cont] | (value *continuations*)])
                 Label))

(define bind-continuations
  [] -> []
  [[Label Vars Body] | Rest] -> [[scm.define [Label | Vars] Body]
                                 | (bind-continuations Rest)])

(define make-label
  Vars [scm.goto-label Continuation | Vars] -> [scm.goto-label Continuation | Vars]
  Vars Body -> (let Label (push-continuation Vars Body)
                 [scm.goto-label Label | Vars]))

(define rebranch-h
  Test Scope TrueBranch FalseBranch Else
  -> (let NewElse (rebranch FalseBranch Scope Else)
          FreeVars (free-variables NewElse Scope)
          GotoElse (make-label FreeVars NewElse)
       [if Test
           (optimize-selectors Test (rebranch TrueBranch Scope GotoElse))
           GotoElse]))

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
  [pos 0 Exp] -> (concat/ (exp-var Exp) hdstr)
  Var -> Var)

(define optimize-selectors
  Test Code -> (bind-repeating-selectors (test->selectors Test) Code))

(define test->selectors
  [cons? X] -> [[hd X] [tl X]]
  [tuple? X] -> [[fst X] [snd X]]
  [string? X] -> [[pos 0 X] [tlstr X]]
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