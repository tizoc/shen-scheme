\* Copyright (c) 2012-2026 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(package port-bench
 [*port-benchmarks* done run]

(define runs-for-power
  _ 0 -> 1
  N 1 -> N
  N P -> (* N (runs-for-power N (- P 1))))

(define reset-benchmarks
  -> (set *port-benchmarks* []))

(define effective-runs-power
  P O -> 0 where (> O P)
  P O -> (- P O))

(define emit-header
  -> (output "mode|sample|tag|description|runs_power|seconds|result~%"))

(define run-one
  Mode Sample O [Tag Desc F P]
  -> (let P* (effective-runs-power P O)
          Runs (runs-for-power 10 P*)
          Start (get-time run)
          Result (F Runs)
          End (get-time run)
          Elapsed (- End Start)
       (do
        (output "~A|~A|~A|~A|~A|~A|~R~%" Mode Sample Tag Desc P* Elapsed Result)
        Elapsed)))

(define run-benchmark-samples
  _ _ _ Sample Samples -> done where (> Sample Samples)
  Mode O B Sample Samples
  -> (do (run-one Mode Sample O B)
         (run-benchmark-samples Mode O B (+ Sample 1) Samples)))

(define run-benchmarks*
  _ _ _ [] -> done
  Mode O Samples [B | Bs]
  -> (do (run-benchmark-samples Mode O B 1 Samples)
         (run-benchmarks* Mode O Samples Bs)))

(define run-benchmarks
  Mode O Samples -> (run-benchmarks* Mode O Samples (reverse (value *port-benchmarks*))))
)
