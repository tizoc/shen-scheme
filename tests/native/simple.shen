\* Copyright (c) 2012-2021 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(define native-test-add
  X Y -> (+ X Y))

(define native-test-inc
  X -> (native-test-add X 1))

(define native-test-sumdown
  0 -> 0
  X -> (+ X (native-test-sumdown (- X 1))))

(define native-test-map-inc
  [] -> []
  [X | Xs] -> [(native-test-inc X) | (native-test-map-inc Xs)])
