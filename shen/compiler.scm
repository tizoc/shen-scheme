(define (unbound-symbol? maybe-sym scope)
  (and (symbol? maybe-sym)
       (not (memq maybe-sym scope))))

(define *gensym-counter* 0)

(define (gensym prefix)
  (set! *gensym-counter* (+ 1 *gensym-counter*))
  (string->symbol (string-append prefix (number->string *gensym-counter*))))

(define (compile-expression expr scope)
  (define (unbound? maybe-sym)
    (unbound-symbol? maybe-sym scope))

  (match expr
    ((? null?) '($$quote ()))
    ('true #t)
    ('false #f)
    ('|{| '($$quote |{|))
    ('|}| '($$quote |}|))
    ('|;| '($$quote |;|))
    ((? unbound? sym) `($$quote ,sym))
    (('let var value body) (emit-let var value body (cons var scope)))
    (('cond clauses ...) (emit-cond clauses scope))
    ;; Remove intermediary wrapper lambdas
    (('lambda var ((? unbound? op) var)) op)
    (('lambda var body)
     `(lambda ,var ,(compile-expression body (cons var scope))))
    (('do expr1 expr2)
     `($$begin ,(compile-expression expr1 scope) ,(compile-expression expr2 scope)))
    (('fail) '($$quote shen.fail!))
    (('$native exp) exp)
    (('$native . exps) `($$begin ,@exps))
    (('= v1 v2) (emit-equality-check v1 v2 scope))
    ((op params ...) (emit-application op params scope))
    (else expr)))

(define (emit-let var value body scope)
  `(let ,var ,(compile-expression value scope)
     ,(compile-expression body (cons var scope))))

(define (emit-cond clauses scope)
  `(cond ,@(emit-cond-clauses clauses scope)))

(define (emit-cond-clauses clauses scope)
  (match clauses
    (() '())
    (((test body) . rest)
     (let ((compiled-test (compile-expression test scope))
           (compiled-body (compile-expression body scope))
           (compiled-rest (emit-cond-clauses rest scope)))
       `((,compiled-test ,compiled-body) ,@compiled-rest)))))

(define (emit-equality-check v1 v2 scope)
  (cond ((or (unbound-symbol? v1 scope)
             (unbound-symbol? v2 scope)
             (equal? '(fail) v1)
             (equal? '(fail) v2))
         `($$eq? ,(compile-expression v1 scope)
                 ,(compile-expression v2 scope)))
        ((or (string? v1) (string? v2))
         `($$equal? ,(compile-expression v1 scope)
                    ,(compile-expression v2 scope)))
        ((null? v1) `($$null? ,(compile-expression v2 scope)))
        ((null? v2) `($$null? ,(compile-expression v1 scope)))
        (else `(= ,(compile-expression v1 scope)
                  ,(compile-expression v2 scope)))))

(define (emit-application op params scope)
  (let* ((arity (function-arity op))
         (partial-call? (not (or (= arity -1) (= arity (length params)))))
         (args (map (lambda (exp) (compile-expression exp scope))
                    params))
         (args-list (left-to-right `($$list ,@args))))
    (cond ((null? args)
           (cond ((pair? op) `(,(compile-expression op scope)))
                 ((unbound-symbol? op scope) `(,op))
                 (else `(($$function-binding ,op)))))
          (partial-call?
           `($$call-nested ,($$nest-lambda op arity) ,args-list))
          ((or (pair? op) (not (unbound-symbol? op scope)))
           (left-to-right
            `($$call-nested ($$function ,(compile-expression op scope)) ,args-list)))
          (else
           (left-to-right (cons op args))))))

(define ($$nest-lambda callable arity)
  (define (merge-args f arg)
    (if (pair? f)
        (append f (list arg))
        (list f arg)))

  (if (<= arity 0)
      callable
      (let ((aname (gensym "Y")))
        `(lambda ,aname
           ,($$nest-lambda (merge-args callable aname) (- arity 1))))))

(define ($$call-nested f args)
  (if (null? args)
      f
      ($$call-nested (f (car args)) (cdr args))))

(define (arity-error? e)
  (string-prefix? "not enough args" (error-object-message e)))

(define (handle-arity-error exn f args)
  (if (and (arity-error? exn) (> (function-arity f) (length args)))
      ($$call-nested
       (kl:eval-kl ($$nest-lambda f (function-arity f))) args)
      (raise exn)))

(define ($$function-binding maybe-symbol)
  (if (symbol? maybe-symbol)
      (hash-table-ref *shen-functions* maybe-symbol
                      (lambda () (error "undefined function: "
                                        maybe-symbol)))
      maybe-symbol))

(define ($$function f)
  (if (not (symbol? f))
      f
      (lambda args
        (call-with-current-continuation
         (lambda (exit)
           (with-exception-handler
            (lambda (exn) (exit (handle-arity-error exn f args)))
            (lambda () (apply ($$function-binding f) args))))))))

;; Enforce left-to-right evaluation if needed
(define (left-to-right expr)
  (if (or (memq (car expr) '(trap-error set and or if freeze thaw))
          (< (length (filter pair? expr)) 2))
      expr
      `($$l2r ,expr ())))

(define-syntax $$l2r
  (syntax-rules ()
    ((_ () ?expr) ?expr)
    ((_ (?op ?params ...) (?expr ...))
     (let ((f ?op))
       ($$l2r (?params ...) (?expr ... f))))))

(define (kl->scheme expr)
  (match expr
    (`(defun ,name ,args ,body)
     ;; pre-register arity in case the function is recursive
     ;; and partially-calls itself
     (register-function-arity name (length args))
     `(defun ,name ,args
        ,(compile-expression body args)))
    (else (compile-expression expr '()))))
