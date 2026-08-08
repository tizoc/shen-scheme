\* Copyright (c) 2012-2021 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(define native-sealed-helper
  X -> (+ X 1))

(define native-sealed-main
  X -> (native-sealed-helper X))
