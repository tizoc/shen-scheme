\* Copyright (c) 2012-2021 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(define native-bench-hop0
  X -> (+ X 1))

(define native-bench-hop1
  X -> (native-bench-hop0 X))

(define native-bench-hop2
  X -> (native-bench-hop1 X))

(define native-bench-hop3
  X -> (native-bench-hop2 X))

(define native-bench-hop4
  X -> (native-bench-hop3 X))

(define native-bench-hop5
  X -> (native-bench-hop4 X))

(define native-bench-hop6
  X -> (native-bench-hop5 X))

(define native-bench-hop7
  X -> (native-bench-hop6 X))

(define native-bench-hop8
  X -> (native-bench-hop7 X))

(define native-bench-hop9
  X -> (native-bench-hop8 X))

(define native-bench-hop10
  X -> (native-bench-hop9 X))

(define native-bench-hop11
  X -> (native-bench-hop10 X))

(define native-bench-entry
  X -> (native-bench-hop11 X))

(define native-bench-loop
  0 Acc -> Acc
  N Acc -> (native-bench-loop (- N 1) (native-bench-entry Acc)))
