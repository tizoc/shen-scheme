(package _scm [scm.goto-label scm.begin scm.define $label hdstr *toplevel*]

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

(define hoist-labels
  [let-label Label LabelBody Body] Scope Acc
  -> (let Vars (free-variables LabelBody Scope)
          NewBody (subst [scm.goto-label Label | Vars] [scm.goto-label Label] Body)
          NewLabelBody+NewAcc (hoist-labels LabelBody Scope Acc)
          Continuation [scm.define [Label | Vars] (fst NewLabelBody+NewAcc)]
          NewAcc [Continuation | (snd NewLabelBody+NewAcc)]
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
  [scm.goto-label Label] F -> (F [scm.goto-label Label])
  Body F -> (let Label (generate-label)
              [let-label Label Body
                (F [scm.goto-label Label])]))

(define rebranch-h
  Test Scope TrueBranch FalseBranch Else
  -> (let NewElse (rebranch FalseBranch Scope Else)
       (with-labelled-else NewElse
         (/. GotoElse
          [if Test
              (optimize-selectors Test (rebranch TrueBranch Scope GotoElse))
              GotoElse]))))

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