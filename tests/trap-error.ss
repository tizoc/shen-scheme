(import (chezscheme) (shen-scheme runtime))

(define (check label result)
  (unless result (error 'trap-error label))
  (printf "[OK]    ~a\n" label))

(define compiled-general-trap
  (kl:eval-kl '(lambda Body (lambda Handler (trap-error (thaw Body) Handler)))))
(define (general-trap body handler)
  ((compiled-general-trap body) handler))

;; Generate while imports are still immutable; install only after the other
;; tests. Later definitions in the same compatible unit must remain visible.
(define compatible-forms
  (kl:shen-scheme.native-compatible-scheme-forms
    '(native-unit
       ((defun value (X) unit-value)
        (defun <-vector (V N) unit-vector)
        (defun unit-value-trap () (trap-error (value unknown) (lambda E default)))
        (defun unit-vector-trap () (trap-error (<-vector unknown 1) (lambda E default))))
       ((set unit-trap-init (trap-error (value unknown) (lambda E default))))
       () ())
    '(runtime)))

(check "general handler preserves normal multiple values"
  (equal? (call-with-values
            (lambda () (general-trap (lambda () (values 1 2)) (lambda (e) #f)))
            list) '(1 2)))
(check "general handler preserves exceptional multiple values"
  (equal? (call-with-values
            (lambda () (general-trap (lambda () (raise 'payload))
                                       (lambda (e) (values e 3))))
            list) '(payload 3)))
(let ((p (make-parameter 'outer)) (events '()))
  (check "handler runs after dynamic-wind and parameter unwinding"
    (eq? (general-trap
           (lambda ()
             (parameterize ((p 'inner))
               (dynamic-wind (lambda () (set! events (cons 'enter events)))
                             (lambda () (raise 'payload))
                             (lambda () (set! events (cons 'leave events))))))
           (lambda (e) (and (eq? e 'payload) (eq? (p) 'outer)
                            (equal? events '(leave enter)) 'unwound)))
         'unwound)))
(check "handler restores continuation marks"
  (eq? (with-continuation-mark 'trap-mark 'outer
         (general-trap
           (lambda () (with-continuation-mark 'trap-mark 'inner (raise 'payload)))
           (lambda (e) (continuation-marks-first (current-continuation-marks) 'trap-mark))))
       'outer))
(check "a handler error reaches the enclosing handler"
  (eq? (general-trap
         (lambda () (general-trap (lambda () (raise 'inner))
                                    (lambda (e) (raise 'outer))))
         (lambda (e) e)) 'outer))
(let ((resume #f) (results '()))
  (let ((result
          (general-trap
            (lambda ()
              (let ((value (call/cc (lambda (k) (set! resume k) 'first))))
                (if (memq value '(raise raise-again)) (raise 'payload) value)))
            (lambda (e) 'caught))))
    (set! results (cons result results))
    (case (length results)
      ((1) (resume 'raise))
      ((2) (resume 'raise-again))
      ((3) (resume 'fourth))
      (else (check "captured continuations can reenter and raise repeatedly"
              (equal? results '(fourth caught caught first)))))))

(define value-default
  (kl:eval-kl '(lambda X (trap-error (value X) (lambda E default)))))
(kl:set 'trap-defined #f)
(check "checked value distinguishes false from an undefined variable"
  (and (eq? (value-default 'trap-defined) #f)
       (eq? (value-default 'trap-undefined) 'default)
       (eq? (value-default 42) 'default)))
(let ((out (open-output-string)))
  (check "checked value reads the current parameter binding"
    (parameterize ((kl:global/*stoutput* out))
      (eq? (value-default '*stoutput*) out))))
(check "checked defaults preserve multiple values"
  (equal? (call-with-values
            (lambda ()
              ((kl:eval-kl '(lambda F (trap-error (value trap-undefined)
                                                  (lambda E (thaw F)))))
               (lambda () (values 1 2))))
            list) '(1 2)))
(define address-default
  (kl:eval-kl '(lambda V (lambda N (trap-error (<-address V N) (lambda E default))))))
(define vector-default
  (kl:eval-kl '(lambda V (lambda N (trap-error (<-vector V N) (lambda E default))))))
(check "checked address preserves zero-index reads"
  (equal? ((address-default '#(stamp 42)) 0) 'stamp))
(check "checked vector rejects zero and uninitialized slots"
  (and (eq? ((vector-default '#(1 shen.fail!)) 0) 'default)
       (eq? ((vector-default '#(1 shen.fail!)) 1) 'default)
       (= ((vector-default '#(1 42)) 1) 42)))
(for-each (lambda (index)
  (check (format "checked reads catch invalid index ~s" index)
    (and (eq? ((address-default '#(0 42)) index) 'default)
         (eq? ((vector-default '#(0 42)) index) 'default))))
  (list -1 2 1.0 #f (expt 2 80)))
(check "checked reads catch invalid container types"
  (and (eq? ((address-default 'not-a-vector) 0) 'default)
       (eq? ((vector-default 'not-a-vector) 1) 'default)))
(check "complex operands keep a handler around their own errors"
  (eq? (kl:eval-kl '(trap-error (<-address (simple-error "operand") 0)
                              (lambda E caught))) 'caught))
(check "lexically bound value is not specialized"
  (eq? ((kl:eval-kl '(lambda value (trap-error (value trap-undefined) (lambda E default))))
        (lambda (x) 'lexical-value)) 'lexical-value))
(check "lexically bound vector accessor is not specialized"
  (eq? ((kl:eval-kl '(lambda <-vector (trap-error (<-vector anything 1) (lambda E default))))
        (lambda (v) (lambda (n) 'lexical-vector))) 'lexical-vector))
(check "prefixed lexical bindings keep their Scheme meaning"
  (eq? ((kl:eval-kl '(lambda kl:value (trap-error (value trap-undefined) (lambda E default))))
        (lambda (x) 'prefixed-value)) 'prefixed-value))
(check "lexically bound Scheme vector-ref is not specialized"
  (eq? ((kl:eval-kl '(lambda vector-ref (trap-error (<-address anything 0) (lambda E default))))
        (lambda (v n) 'lexical-address)) 'lexical-address))
(check "explicit Scheme definitions preserve imported-name shadowing"
  (eq? ((kl:eval-kl
          '(lambda X
             (do (scm.define (vector-ref V N)
                   (if (= N 0) scheme-address
                       (trap-error (<-address V 0) (lambda E default))))
                 (scm.vector-ref anything 1))))
        #f) 'scheme-address))
(check "generated helper names do not capture lexical bindings"
  (eq? ((kl:eval-kl '(lambda shen-value/or
                       (trap-error (value trap-defined) (lambda E default))))
        (lambda args 'wrong-helper)) #f))
(define (trap-local-value x) 'native-value)
(define native-value-code
  (kl:_scm.with-native-context 'sealed '((value trap-local-value))
    (lambda () (kl:_scm.kl->scheme
                 '(lambda X (trap-error (value X) (lambda E default)))))))
(check "native local value is not specialized"
  (eq? ((eval native-value-code) 'anything) 'native-value))
(check "raw Scheme handlers can still inspect the exception"
  (eq? (kl:eval-kl '(trap-error (value trap-undefined)
                               (lambda E (scm. "(condition? E)")))) #t))
(check "error binders cannot hide primitive references in a handler"
  (eq? (kl:eval-kl
         '(trap-error (value trap-undefined)
            (lambda vector-ref
              (trap-error (<-address #(42) 0) (lambda E inner-default)))))
       'inner-default))
(define (E) 'mapped-handler)
(define mapped-handler-code
  (kl:_scm.with-native-context 'sealed '((mapped-handler E))
    (lambda () (kl:_scm.kl->scheme
                 '(trap-error (value trap-undefined) (lambda E (mapped-handler)))))))
(check "error binders preserve native local-name capture"
  (guard (e (else #t)) (eval mapped-handler-code) #f))
(kl:set '_scm.*compiling-shen-sources* #t)
(define kernel-condition
  (kl:eval-kl '(trap-error (value trap-undefined) (lambda E E))))
(kl:set '_scm.*compiling-shen-sources* #f)
(check "kernel shortcut requires an unused condition" (condition? kernel-condition))

(define original-value kl:value)
(kl:eval-kl '(defun value (X) replacement))
(check "earlier checked value retains the original binding"
  (eq? (value-default 'trap-undefined) 'default))
(check "later checked value observes redefinition"
  (eq? (kl:eval-kl '(trap-error (value trap-undefined) (lambda E default))) 'replacement))
(define-top-level-value 'kl:value original-value)
(define live-value
  (kl:eval-kl '(lambda X (trap-error (value X) (lambda E default)))))
(kl:eval-kl '(defun value (X) alias-replacement))
(check "mutable value aliases retain live calls"
  (eq? (live-value 'trap-undefined) 'alias-replacement))
(define-top-level-value 'kl:value original-value)
(define original-vector kl:<-vector)
(kl:eval-kl '(defun <-vector (V N) replacement))
(check "earlier checked vector retains the original binding"
  (= ((vector-default '#(1 42)) 1) 42))
(check "later checked vector observes redefinition"
  (eq? (kl:eval-kl '(trap-error (<-vector not-a-vector 1) (lambda E default))) 'replacement))
(define-top-level-value 'kl:<-vector original-vector)
(define live-vector
  (kl:eval-kl '(lambda X (trap-error (<-vector X 1) (lambda E default)))))
(kl:eval-kl '(defun <-vector (V N) alias-replacement))
(check "mutable vector aliases retain live calls"
  (eq? (live-vector '#(1 42)) 'alias-replacement))
(define-top-level-value 'kl:<-vector original-vector)

(define original-vector-ref vector-ref)
(define-top-level-value 'vector-ref original-vector-ref)

(for-each eval compatible-forms)
(check "compatible units preserve their own value definition"
  (eq? ((top-level-value 'kl:unit-value-trap)) 'unit-value))
(check "compatible units preserve their own vector definition"
  (eq? ((top-level-value 'kl:unit-vector-trap)) 'unit-vector))
(check "compatible initializers observe unit definitions"
  (eq? (original-value 'unit-trap-init) 'unit-value))
(define-top-level-value 'kl:value original-value)
(define-top-level-value 'kl:<-vector original-vector)
(define live-address
  (kl:eval-kl '(lambda X (trap-error (<-address X 0) (lambda E default)))))
(define-top-level-value 'vector-ref (lambda (v n) 'scheme-replacement))
(check "mutable Scheme vector-ref aliases retain live calls"
  (eq? (live-address '#(42)) 'scheme-replacement))
(define-top-level-value 'vector-ref original-vector-ref)
