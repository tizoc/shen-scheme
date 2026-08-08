\* Copyright (c) 2012-2026 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(package realistic-bench
 [const load add-op sub-op mul-op bind unbind]

(define run-code
  Code E -> (exec Code [] E))

(define push-const
  N S -> [N | S])

(define push-load
  N S E -> [(env-lookup N E) | S])

(define add-values
  L R -> (+ L R))

(define sub-values
  L R -> (- L R))

(define mul-values
  L R -> (* L R))

(define apply-add
  L R S -> [(add-values L R) | S])

(define apply-sub
  L R S -> [(sub-values L R) | S])

(define apply-mul
  L R S -> [(mul-values L R) | S])

(define push-binding
  N V E -> [[N V] | E])

(define drop-binding
  [_ | E] -> E
  [] -> [])

(define exec
  [] [R | _] _ -> R
  [[const N] | Is] S E -> (exec Is (push-const N S) E)
  [[load N] | Is] S E -> (exec Is (push-load N S E) E)
  [[add-op] | Is] [R L | S] E -> (exec Is (apply-add L R S) E)
  [[sub-op] | Is] [R L | S] E -> (exec Is (apply-sub L R S) E)
  [[mul-op] | Is] [R L | S] E -> (exec Is (apply-mul L R S) E)
  [[bind N] | Is] [V | S] E -> (exec Is S (push-binding N V E))
  [[unbind] | Is] S E -> (exec Is S (drop-binding E))
  [Op | _] S E -> (error "realistic benchmark VM failed at ~S with stack ~S and env ~S~%" Op S E))
)
