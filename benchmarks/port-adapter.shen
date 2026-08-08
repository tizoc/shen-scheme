\* Copyright (c) 2012-2026 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(package port-bench
 [*port-benchmarks* add-benchmark done]

(define add-benchmark
  Tag Desc F P -> (do (set *port-benchmarks* [[Tag Desc F P] | (value *port-benchmarks*)])
                      done))
)
