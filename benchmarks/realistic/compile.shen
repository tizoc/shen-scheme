\* Copyright (c) 2012-2026 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(package realistic-bench
 [num var add sub mul let1 const load add-op sub-op mul-op bind unbind]

(define compile-ast
  [num N] -> [[const N]]
  [var N] -> [[load N]]
  [add A B] -> (append (compile-ast A) (compile-ast B) [[add-op]])
  [sub A B] -> (append (compile-ast A) (compile-ast B) [[sub-op]])
  [mul A B] -> (append (compile-ast A) (compile-ast B) [[mul-op]])
  [let1 N V B] -> (append (compile-ast V) [[bind N]] (compile-ast B) [[unbind]])
  X -> (error "realistic benchmark cannot compile expression: ~S~%" X))
)
