\* Copyright (c) 2012-2026 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(package realistic-bench
 [*port-benchmarks* add-benchmark realistic-pipeline realistic-vm]

(set *port-benchmarks* [])

(add-benchmark realistic-pipeline "compile pipeline depth 5" (run-pipeline 5 2) 4)
(add-benchmark realistic-pipeline "compile pipeline depth 7" (run-pipeline 7 3) 4)
(add-benchmark realistic-pipeline "compile pipeline depth 9" (run-pipeline 9 3) 3)

(add-benchmark realistic-vm "bytecode VM depth 7" (run-bytecode 7 3) 6)
(add-benchmark realistic-vm "bytecode VM depth 9" (run-bytecode 9 3) 6)
)
