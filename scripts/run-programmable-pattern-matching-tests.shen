(define ppm-test.assert-equal
  Label Expected Expected -> (output "ok - ~A~%" Label)
  Label Expected Actual -> (error "~A: expected ~R, got ~R~%" Label Expected Actual))

(define ppm-test.two-handler
  Self AddTest Bind [ppm-test.two A B]
  -> (do (AddTest [tuple? Self])
         (Bind A [fst Self])
         (Bind B [snd Self]))
  _ _ _ _ -> (fail))

(ppm-test.assert-equal
  "custom pattern compiler initialized at startup"
  true
  (not (= false (value shen.*custom-pattern-compiler*))))

(ppm-test.assert-equal
  "custom pattern reducer initialized at startup"
  true
  (not (= false (value shen.*custom-pattern-reducer*))))

(ppm-test.assert-equal
  "handler registry initialized at startup"
  []
  (value shen.x.programmable-pattern-matching.*pattern-handlers-reg*))

(shen.x.programmable-pattern-matching.register-handler ppm-test.two-handler)
(shen.x.programmable-pattern-matching.register-handler ppm-test.two-handler)

(ppm-test.assert-equal
  "handler registration is idempotent"
  [ppm-test.two-handler]
  (value shen.x.programmable-pattern-matching.*pattern-handlers-reg*))

(load "tests/programmable-pattern-matching/code.shen")

(ppm-test.assert-equal
  "simple custom pattern"
  [1 2]
  (ppm-test.match-simple (@p 1 2)))

(ppm-test.assert-equal
  "repeated variable success"
  same
  (ppm-test.match-repeat (@p 1 1)))

(ppm-test.assert-equal
  "repeated variable failure"
  different
  (ppm-test.match-repeat (@p 1 2)))

(ppm-test.assert-equal
  "nested custom pattern"
  [1 2 3]
  (ppm-test.match-nested (@p (@p 1 2) 3)))

(ppm-test.assert-equal
  "built-in patterns still work"
  [1 [2 3]]
  (ppm-test.match-cons [1 2 3]))

(ppm-test.assert-equal
  "handler can be unregistered"
  ppm-test.two-handler
  (shen.x.programmable-pattern-matching.unregister-handler ppm-test.two-handler))

(ppm-test.assert-equal
  "handler registry is empty after unregister"
  []
  (value shen.x.programmable-pattern-matching.*pattern-handlers-reg*))

(load "tests/programmable-pattern-matching/after-unregister.shen")

(ppm-test.assert-equal
  "compiled custom patterns keep their behavior"
  [1 2]
  (ppm-test.match-simple (@p 1 2)))

(ppm-test.assert-equal
  "unregistered patterns no longer match custom values"
  no
  (ppm-test.match-after-unregister (@p 1 2)))

(ppm-test.assert-equal
  "unregistered patterns retain ordinary list semantics"
  [1 2]
  (ppm-test.match-after-unregister [ppm-test.two 1 2]))
