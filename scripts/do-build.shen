\\ Overrides

(package shen []

(define scm-call-h?
  (@s "scm." _) -> true
  _ -> false)

(define scm-call?
  F -> (scm-call-h? (str F)) where (symbol? F)
  _ -> false)

(set *platform-native-call-check* (/. F (scm-call? F)))

(define platform-native-call?
  F -> (let Check (value *platform-native-call-check*)
         (Check F)))

(define process-application
  [F | X] Types -> (let ArityF (arity F)
                        N (length X)
                        (cases (element? [F | X] Types)           [F | X]
                               (shen-call? F)                     [F | X]
                               (platform-native-call? F)          [F | X]
                               (fn-call? [F | X])                 (fn-call [F | X])
                               (zero-place? [F | X])              [F | X]
                               (undefined-f? F ArityF)            (simple-curry [[fn F] | X])
                               (variable? F)                      (simple-curry [F | X])
                               (application? F)                   (simple-curry [F | X])
                               (partial-application*? F ArityF N) (lambda-function [F | X] (- ArityF N))
                               (overapplication? F ArityF N)      (simple-curry [F | X])
                               true                               [F | X])))

)

(load "scripts/build.shen")

(build program "shen-scheme.scm")