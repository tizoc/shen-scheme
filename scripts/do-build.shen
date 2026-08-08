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

(define bootstrap.source-rules
  [{ | Rest] -> (bootstrap.source-rules-after-signature Rest)
  Rules -> Rules)

(define bootstrap.source-rules-after-signature
  [} | Rules] -> Rules
  [_ | Rest] -> (bootstrap.source-rules-after-signature Rest))

(define bootstrap.source-rule-arity
  [Arrow | _] -> 0 where (= Arrow (intern "->"))
  [Arrow | _] -> 0 where (= Arrow (intern "<-"))
  [_ | Rest] -> (+ 1 (bootstrap.source-rule-arity Rest)))

(define bootstrap.register-source-arities
  [] -> []
  [[define Name | Rules] | Rest]
  -> (do (update-lambda-table
          Name
          (bootstrap.source-rule-arity
           (bootstrap.source-rules Rules)))
         (bootstrap.register-source-arities Rest))
  [_ | Rest] -> (bootstrap.register-source-arities Rest))

(bootstrap.register-source-arities (read-file "src/compiler.shen"))

(load "scripts/build.shen")

(build program "shen-scheme.scm")
