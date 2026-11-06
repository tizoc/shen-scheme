(import (chezscheme) (shen-scheme runtime))

;; Run in its own initialized process: raw eval-kl deliberately shadows imports.
(define (check label result)
  (unless result (error 'primitive-specialization label))
  (printf "[OK]    ~a\n" label))

(define original-equality kl:=)
(define literal-before (kl:eval-kl '(lambda X (= X 5))))
(define general-before (kl:eval-kl '(lambda X (lambda Y (= X Y)))))
(check "numeric literal accepts an inexact result" (literal-before (* 2.5 2)))
(check "numeric literal rejects a nonnumber" (not (literal-before "5")))
(check "generic equality accepts a bignum" (kl:= (expt 2 80) (expt 2 80)))
(check "generic equality rejects distinct bignums" (not (kl:= (expt 2 80) (+ 1 (expt 2 80)))))
(check "numeric literal retains Scheme numeric equality"
  (literal-before (make-rectangular 5.0 0.0)))

(kl:eval-kl '(defun = (X Y) replacement))
(check "earlier literal code retains original equality" (literal-before 5.0))
(check "earlier generic code retains original equality" ((general-before 5) 5.0))
(check "later numeric literal code sees redefinition"
  (eq? (kl:eval-kl '(= 5 5)) 'replacement))
(check "later numeric literal on the left sees redefinition"
  (eq? ((kl:eval-kl '(lambda X (= 5 X))) 5.0) 'replacement))
(check "kernel equality stays bound" (not (kl:element? 0 '(1 2 3))))
(define-top-level-value 'kl:= original-equality)

(define original-check kl:_scm.assert-boolean)
(define boolean-before (kl:eval-kl '(lambda X (if X yes no))))
(kl:eval-kl '(defun _scm.assert-boolean (X) #f))
(check "earlier Boolean check still rejects nonbooleans"
  (guard (e [else (equal? (kl:error-to-string e)
                         "expected a boolean in if/and/or expression, got: 42")])
    (boolean-before 42)
    #f))
(check "later Boolean check sees redefinition"
  (eq? ((kl:eval-kl '(lambda X (if X yes no))) 42) 'no))
(define-top-level-value 'kl:_scm.assert-boolean original-check)
