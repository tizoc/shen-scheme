\* Copyright (c) 2012-2021 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(package native-bench
 [compatible sealed native-bench-loop
  shen-scheme.compile-file/emit/mode
  shen-scheme.load-compiled]

(define iterations
  -> 20000000)

(define warmup-iterations
  -> 1000000)

(define samples
  -> 5)

(define compile-load
  Source Object Scheme Mode
  -> (do (shen-scheme.compile-file/emit/mode Source Object Scheme Mode)
         (shen-scheme.load-compiled Object)))

(define workload
  N -> (eval [native-bench-loop N 0]))

(define time-sample
  Label Sample -> (let Start (get-time run)
                       Result (workload (iterations))
                       End (get-time run)
                       Elapsed (- End Start)
                    (do (output "~A sample ~A: ~A seconds (result ~A)~%" Label Sample Elapsed Result)
                        Elapsed)))

(define run-samples
  _ Sample Total -> [] where (> Sample Total)
  Label Sample Total -> [(time-sample Label Sample) | (run-samples Label (+ Sample 1) Total)])

(define best-time
  [Time | Times] -> (best-time* Time Times))

(define best-time*
  Best [] -> Best
  Best [Time | Times] -> (best-time* Time Times) where (< Time Best)
  Best [_ | Times] -> (best-time* Best Times))

(define run-one
  Label Mode Object Scheme
  -> (do (compile-load "tests/native/call-heavy.shen" Object Scheme Mode)
         (output "~A warmup: ~A iterations~%" Label (warmup-iterations))
         (workload (warmup-iterations))
         (let Times (run-samples Label 1 (samples))
              Best (best-time Times)
           (do (output "~A best: ~A seconds~%" Label Best)
               Best))))

(define main
  -> (do (output "native benchmark iterations/sample: ~A~%" (iterations))
         (output "native benchmark samples: ~A~%" (samples))
         (let Compatible (run-one "compatible" compatible "_build/native-bench/call-heavy-compatible.so" "_build/native-bench/call-heavy-compatible.scm")
              Sealed (run-one "sealed" sealed "_build/native-bench/call-heavy-sealed.so" "_build/native-bench/call-heavy-sealed.scm")
           (output "sealed speedup over compatible: ~A x~%" (/ Compatible Sealed)))))

(main)
)
