\* Copyright (c) 2012-2026 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(package realistic-bench
 []

(define pipeline
  D P S -> (let Program (make-program D)
                 Normal (normalise Program)
                 Vars (free-vars Normal)
                 Optimised (optimise-ast Normal P)
                 Code (compile-ast Optimised)
                 Value (run-code Code (initial-env S))
              (+ Value (list-length Vars) (ast-size Optimised))))

(define pipeline-loop
  _ _ _ Result 0 -> Result
  D P S _ N -> (pipeline-loop D P (+ S 1) (pipeline D P S) (- N 1)))

(define run-pipeline
  D P N -> (pipeline-loop D P 0 0 N))

(define prepare-code
  D P -> (let Program (make-program D)
              Optimised (optimise-ast (normalise Program) P)
           (compile-ast Optimised)))

(define bytecode-loop
  _ _ Result 0 -> Result
  Code S _ N -> (bytecode-loop Code (+ S 1) (run-code Code (initial-env S)) (- N 1)))

(define run-bytecode
  D P N -> (let Code (prepare-code D P)
             (bytecode-loop Code 0 0 N)))
)
