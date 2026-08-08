\* Copyright (c) 2012-2026 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(package realistic-bench
 [num var add sub mul let1]

(define free-vars
  [num _] -> []
  [var N] -> [N]
  [add A B] -> (union-vars (free-vars A) (free-vars B))
  [sub A B] -> (union-vars (free-vars A) (free-vars B))
  [mul A B] -> (union-vars (free-vars A) (free-vars B))
  [let1 N V B] -> (union-vars (free-vars V) (remove-var N (free-vars B)))
  _ -> [])

(define ast-size
  [num _] -> 1
  [var _] -> 1
  [add A B] -> (+ 1 (ast-size A) (ast-size B))
  [sub A B] -> (+ 1 (ast-size A) (ast-size B))
  [mul A B] -> (+ 1 (ast-size A) (ast-size B))
  [let1 _ V B] -> (+ 1 (ast-size V) (ast-size B))
  _ -> 0)
)
