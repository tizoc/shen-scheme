\* Copyright (c) 2012-2026 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(package realistic-bench
 [num var add sub mul let1]

(define member?
  _ [] -> false
  X [X | _] -> true
  X [_ | Xs] -> (member? X Xs))

(define add-unique
  X Xs -> Xs where (member? X Xs)
  X Xs -> [X | Xs])

(define union-vars
  [] Ys -> Ys
  [X | Xs] Ys -> (union-vars Xs (add-unique X Ys)))

(define remove-var
  _ [] -> []
  X [X | Xs] -> (remove-var X Xs)
  X [Y | Ys] -> [Y | (remove-var X Ys)])

(define list-length
  Xs -> (list-length* Xs 0))

(define list-length*
  [] N -> N
  [_ | Xs] N -> (list-length* Xs (+ N 1)))

(define env-lookup
  _ [] -> 0
  N [[N V] | _] -> V
  N [_ | E] -> (env-lookup N E))

(define initial-env
  S -> [[0 (+ S 1)]])

(define make-program
  D -> [let1 0 [num 7] (make-block D 1)])

(define make-block
  0 S -> [add [var 0] [num S]]
  D S -> [let1 S
               (make-expr D S)
               [add (make-block (- D 1) (+ S 1))
                    [mul [var S] [num (+ S 3)]]]])

(define make-expr
  0 S -> [add [var 0] [num S]]
  D S -> [add [mul [var (- S 1)] [num (+ D S)]]
              [sub [num (* D S)] [var 0]]])
)
