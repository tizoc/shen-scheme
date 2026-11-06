(import (chezscheme) (shen-scheme runtime))

(define (check label result)
  (unless result (error 'kernel-initialization label))
  (printf "[OK]    ~a\n" label))

(define properties (kl:global/*property-vector*))
(define (adapter name) (kl:get name 'shen.lambda-form properties))
(define (no-eval . args)
  (error 'kernel-initialization "immutable kernel registration called eval"))

(parameterize ([current-eval no-eval])
  (check "kernel adapter registration avoids eval"
    (eq? (kl:shen.update-lambda-table/precompiled 'not 1 (lambda () (lambda (x) (kl:not x)))
      (shen-precompiled-function? 'not kl:not)) 'not))
  (kl:shen.update-lambda-table/precompiled 'hash 2
    (lambda () (lambda (x) (lambda (n) (kl:hash x n))))
    (shen-precompiled-function? 'hash kl:hash))
  (kl:put 'fail 'shen.lambda-form 'stale properties)
  (kl:shen.update-lambda-table/precompiled 'fail 0 (lambda () #f)
    (shen-precompiled-function? 'fail kl:fail)))
(check "unary adapter preserves its result" ((adapter 'not) #f))
(check "binary adapter preserves currying"
  (= (((adapter 'hash) "key") 97) (kl:hash "key" 97)))
(check "zero arity removes an obsolete adapter"
  (guard (e [else #t]) (adapter 'fail) #f))
(check "arity metadata is installed" (= (kl:arity 'hash) 2))

;; Even an alias to the original procedure is mutable and must keep live calls.
(define original-not kl:not)
(define-top-level-value 'kl:not original-not)
(kl:shen.update-lambda-table/precompiled 'not 1
  (lambda () (error 'kernel-initialization "fallback constructed an adapter"))
  (shen-precompiled-function? 'not original-not))
(define live-not (adapter 'not))
(check "mutable alias uses the ordinary registration path" (live-not #f))
(kl:eval-kl '(defun not (X) replacement))
(check "adapter observes a later redefinition" (eq? (live-not #f) 'replacement))
(kl:shen.update-lambda-table/precompiled 'not 1
  (lambda () (error 'kernel-initialization "fallback constructed an adapter"))
  (shen-precompiled-function? 'not original-not))
(check "shadowed kernel binding uses the replacement"
  (eq? ((adapter 'not) #f) 'replacement))
(define-top-level-value 'kl:not original-not)

(kl:shen.update-lambda-table/precompiled 'initialization-future 1
  (lambda () (error 'kernel-initialization "unbound name constructed an adapter"))
  (shen-precompiled-function? 'initialization-future original-not))
(kl:eval-kl '(defun initialization-future (X) (+ X 1)))
(check "unbound names retain forward references"
  (= ((adapter 'initialization-future) 41) 42))
