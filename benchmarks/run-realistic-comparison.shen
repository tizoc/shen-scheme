\* Copyright (c) 2012-2026 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(package realistic-bench
 [*argv* dynamic compatible sealed app app-wpo done
  scm. file-exists? mkdir string->number
  port-bench.emit-header
  port-bench.run-benchmarks
  shen-scheme.compile-module
  shen-scheme.build-module-app/profile
  shen-scheme.build-module-app/wpo/profile
  shen-scheme.load-compiled]

(define sources
  -> ["benchmarks/realistic/core.shen"
      "benchmarks/realistic/analysis.shen"
      "benchmarks/realistic/optimize.shen"
      "benchmarks/realistic/compile.shen"
      "benchmarks/realistic/vm.shen"
      "benchmarks/realistic/main.shen"])

(define harness-source
  -> "benchmarks/port-harness.shen")

(define adapter-source
  -> "benchmarks/port-adapter.shen")

(define registration-source
  -> "benchmarks/realistic-registrations.shen")

(define module-dir
  -> "benchmarks/realistic")

(define output-dir
  -> "_build/native-bench")

(define object-path
  Mode -> (make-string "~A/realistic-bench-~A.so" (output-dir) Mode))

(define descriptor
  compatible -> "benchmarks/realistic/realistic.bench.whole-compatible.shenmod"
  sealed -> "benchmarks/realistic/realistic.bench.whole-sealed.shenmod"
  app -> "benchmarks/realistic/realistic.bench.main.shenmod"
  app-wpo -> "benchmarks/realistic/realistic.bench.main.shenmod")

(define ensure-directory
  D -> D where (eval-kl [scm. [file-exists? D]])
  D -> (do (eval-kl [scm. [mkdir D]]) D))

(define ensure-output-directory
  -> (do (ensure-directory "_build") (ensure-directory (output-dir))))

(define load-sources
  [] -> loaded
  [Source | Sources] -> (do (load Source) (load-sources Sources)))

(define script-args
  -> (if (bound? *argv*)
         (tl (value *argv*))
         []))

(define arg-mode
  "dynamic" -> dynamic
  "compatible" -> compatible
  "sealed" -> sealed
  "app" -> app
  "app-wpo" -> app-wpo
  _ -> (fail))

(define arg-modes
  [] -> []
  [Arg | Args] -> (let Mode (arg-mode Arg)
                    (if (= Mode (fail))
                        (arg-modes Args)
                        [Mode | (arg-modes Args)])))

(define modes
  Args -> (let Modes (arg-modes Args)
            (if (= Modes [])
                [dynamic compatible sealed app app-wpo]
                Modes)))

(define string->number*
  S -> (eval-kl [scm. [string->number S]]))

(define option-number
  Flag S -> (let N (string->number* S)
              (if (= N false)
                  (error "~A expected a number, got: ~A~%" Flag S)
                  N)))

(define quiet
  F -> (let Old (value *hush*)
            _ (set *hush* true)
            X (thaw F)
            _ (set *hush* Old)
         X))

(define arg-offset
  [] -> 0
  ["--quick" | _] -> 4
  ["quick" | _] -> 4
  ["--offset" O | _] -> (option-number "--offset" O)
  [_ | Args] -> (arg-offset Args))

(define arg-samples
  [] -> 1
  ["--samples" N | _] -> (option-number "--samples" N)
  [_ | Args] -> (arg-samples Args))

(define prepare-mode
  dynamic -> (load-sources (sources))
  compatible -> (let O (object-path compatible)
                  (do (shen-scheme.compile-module (descriptor compatible) O)
                      (shen-scheme.load-compiled O)))
  sealed -> (let O (object-path sealed)
              (do (shen-scheme.compile-module (descriptor sealed) O)
                  (shen-scheme.load-compiled O)))
  app -> (let O (object-path app)
           (do (shen-scheme.build-module-app/profile (descriptor app) (module-dir) O release)
               (shen-scheme.load-compiled O)))
  app-wpo -> (let O (object-path app-wpo)
               (do (shen-scheme.build-module-app/wpo/profile (descriptor app-wpo) (module-dir) O release)
                   (shen-scheme.load-compiled O)))
  Mode -> (error "unknown benchmark mode: ~A~%" Mode))

(define run-mode
  Mode O N -> (do (output "# preparing mode ~A~%" Mode)
                  (quiet (freeze (do (prepare-mode Mode) (load (registration-source)))))
                  (port-bench.run-benchmarks Mode O N)))

(define run-modes
  [] _ _ -> done
  [Mode | Modes] O N -> (do (run-mode Mode O N) (run-modes Modes O N)))

(define main
  -> (let Args (script-args)
          Modes (modes Args)
          O (arg-offset Args)
          N (arg-samples Args)
       (do (ensure-output-directory)
           (quiet (freeze (do (load (adapter-source)) (load (harness-source)))))
           (output "# modes: ~R~%" Modes)
           (output "# run_power_offset: ~A~%" O)
           (output "# samples: ~A~%" N)
           (port-bench.emit-header)
           (run-modes Modes O N))))

(main)
)
