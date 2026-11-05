\\ Regressions for explicit cond, numeric literal equality, and intern.
(set _scm.*compiling-shen-sources* false)

(define semantics-cond X -> (cond ((= X 1) a) ((= X 2) b)))
(define semantics-nested-cond X -> (let Y X (cond ((= Y 1) a))))

(assert-equal (semantics-cond 1) a)
(assert-equal (semantics-cond 2) b)
(assert-equal
 (trap-error (semantics-cond 3) (/. E (error-to-string E)))
 "condition failure")
(assert-equal
 (trap-error (semantics-nested-cond 2) (/. E (error-to-string E)))
 "condition failure")
(assert-equal
 (trap-error (eval-kl [cond]) (/. E (error-to-string E)))
 "condition failure")
(assert-equal
 (trap-error (eval-kl [cond [false 1]]) (/. E (error-to-string E)))
 "condition failure")
(assert-equal
 (cond (true ok) ((simple-error "unreachable test") unreachable))
 ok)
(assert-equal
 (trap-error (eval-kl [cond [42 yes] [true no]]) (/. E caught))
 caught)

(define semantics-eq-right X -> (= X 1))
(define semantics-eq-left X -> (= 1 X))
(define semantics-eq-params X Y -> (= X Y))

(let Real (+ 0.5 0.5)
  (assert-equal
   [(= 1 Real) (semantics-eq-right Real) (semantics-eq-left Real)
    (semantics-eq-params 1 Real)]
   [true true true true]))
(assert-equal
 (map (fn semantics-eq-right) [1 2 one "1" [1] []])
 [true false false false false false])
(assert-equal
 (map (fn semantics-eq-left) [1 2 one "1" [1] []])
 [true false false false false false])

(set *semantics-eq-calls* 0)
(define semantics-eq-value
 -> (do (set *semantics-eq-calls* (+ 1 (value *semantics-eq-calls*)))
        (+ 0.5 0.5)))
(assert-equal (= 1 (semantics-eq-value)) true)
(assert-equal (value *semantics-eq-calls*) 1)

(define semantics-pattern 1 -> matched _ -> missed)
(assert-equal (semantics-pattern (+ 0.5 0.5)) matched)

(define semantics-intern Name -> (intern Name))
(assert-equal
 (map (/. Name (number? (semantics-intern Name)))
      ["1+2i" "1@2" "+i" "12" "1.5" "1e2"])
 [false false false true true true])
(assert-equal (number? (intern "1+2i")) false)
(assert-equal (str (intern "1+2i")) "1+2i")
(assert-equal (number? (intern "1@2")) false)
(assert-equal (str (intern "1@2")) "1@2")
(assert-equal (number? (intern "+i")) false)
(assert-equal (intern "12") 12)
(assert-equal (intern "1.5") 1.5)
(assert-equal (intern "1e2") 100)
(assert-equal (intern "true") true)
(assert-equal (intern "false") false)
