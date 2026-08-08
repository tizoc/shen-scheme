\* Copyright (c) 2012-2026 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(package realistic-bench
 [num var add sub mul let1]

(define normalise
  [num N] -> [num N]
  [var N] -> [var N]
  [add A B] -> [add (normalise A) (normalise B)]
  [sub A B] -> [add (normalise A) [mul [num -1] (normalise B)]]
  [mul A B] -> [mul (normalise A) (normalise B)]
  [let1 N V B] -> [let1 N (normalise V) (normalise B)]
  X -> X)

(define simplify
  [num N] -> [num N]
  [var N] -> [var N]
  [add A B] -> (simplify-add (simplify A) (simplify B))
  [sub A B] -> (simplify [add A [mul [num -1] B]])
  [mul A B] -> (simplify-mul (simplify A) (simplify B))
  [let1 N V B] -> (simplify-let N (simplify V) (simplify B))
  X -> X)

(define simplify-add
  [num 0] B -> B
  A [num 0] -> A
  [num A] [num B] -> [num (+ A B)]
  A B -> [add A B])

(define simplify-mul
  [num 0] _ -> [num 0]
  _ [num 0] -> [num 0]
  [num 1] B -> B
  A [num 1] -> A
  [num A] [num B] -> [num (* A B)]
  A B -> [mul A B])

(define simplify-let
  N V B -> [let1 N V B] where (member? N (free-vars B))
  _ _ B -> B)

(define optimise-ast
  X 0 -> X
  X N -> (optimise-ast (simplify (normalise X)) (- N 1)))
)
