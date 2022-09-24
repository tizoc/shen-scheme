\\ Overrides

(package shen []

(define process-application
  [F | X] Types -> (let ArityF (arity F)
                        N (length X)
                        (cases (element? [F | X] Types)           [F | X]
                               (shen-call? F)                     [F | X]
                               (foreign? [F | X])                 (unpack-foreign [F | X])
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