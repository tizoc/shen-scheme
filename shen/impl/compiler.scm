;; Copyright (c) 2012-2015 Bruno Deferrari.  All rights reserved.
;; BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(define (call-with-input-string str proc)
  (let ((in (open-input-string str)))
    (proc in)))

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

  (define (ce expr . extra-scope)
    (compile-expression expr (append extra-scope scope)))

  (match expr
    ((? null?) '(scm.quote ()))
    ('true #t)
    ('false #f)
    ('|{| '(scm.quote |{|))
    ('|}| '(scm.quote |}|))
    ('|;| '(scm.quote |;|))
    ((? unbound? sym) `(scm.quote ,sym))
    (('let var value body) (emit-let var value body (cons var scope)))
    (('cond clauses ...) (emit-cond clauses scope))
    ;; Remove intermediary wrapper lambdas
    ;; FIXME: disabled for now, op may not be defined yet
    ;; (('lambda var ((? unbound? op) var)) op)
    (('lambda var body)
     `(scm.lambda (,var) ,(ce body var)))
    (('and expr1 expr2)
     `(scm.and (scm.assert-boolean ,(ce expr1))
               (scm.assert-boolean ,(ce expr2))))
    (('or expr1 expr2)
     `(scm.or (scm.assert-boolean ,(ce expr1))
              (scm.assert-boolean ,(ce expr2))))
    (('if test then else)
     `(scm.if (scm.assert-boolean ,(ce test))
              ,(ce then)
              ,(ce else)))
    (('trap-error expression ('lambda (v) body))
     `(scm.guard (?exn
                  (scm.else (scm.let ((,v ?exn))
                              ,(ce body v))))
        ,(ce expression)))
    (('trap-error expression handler)
     `(scm.let ((?handler ,(ce handler)))
        (scm.guard (?exn (scm.else (?handler ?exn)))
          ,(ce expression))))
    (('do expr1 expr2) `(scm.begin ,(ce expr1) ,(ce expr2)))
    (('freeze expr) `(scm.lambda () ,(ce expr)))
    (('fail) '(scm.quote shen.fail!))
    (('type x type) (ce x))
    (('scm. exp) (call-with-input-string exp read))
    (('= v1 v2) (emit-equality-check v1 v2 scope))
    (('/ a b) (left-to-right `(scm./ (scm.inexact ,(ce a)) ,(ce b))))
    (('pos str n)
     `(scm.string ,(left-to-right `(scm.string-ref ,(ce str) ,(ce n)))))
    (('tlstr str) `(scm.substring ,(ce str) 1))
    (('cn str1 str2)
     (left-to-right `(scm.string-append ,(ce str1) ,(ce str2))))
    (('n->string n) `(scm.string (scm.integer->char ,(ce n))))
    (('string->n str) `(scm.char->integer (scm.string-ref ,(ce str) 0)))
    (('absvector n) `(scm.make-vector ,(ce n) (scm.quote shen.fail!)))
    (('<-address v n)
     (left-to-right `(scm.vector-ref ,(ce v) ,(ce n))))
    (('address-> v n x)
     `(scm.let ((_tmp ,(ce v)))
       ,(left-to-right `(scm.vector-set! _tmp ,(ce n) ,(ce x)))
       _tmp))
    ((op params ...) (emit-application op params scope))
    (else expr)))

(define (emit-let var value body scope)
  `(scm.let ((,var ,(compile-expression value scope)))
     ,(compile-expression body (cons var scope))))

(define (emit-cond clauses scope)
  `(scm.cond ,@(emit-cond-clauses clauses scope)))

(define (emit-cond-clauses clauses scope)
  (match clauses
    (() '())
    (((test body) . rest)
     (let ((compiled-test (compile-expression test scope))
           (compiled-body (compile-expression body scope))
           (compiled-rest (emit-cond-clauses rest scope)))
       `(((scm.assert-boolean ,compiled-test) ,compiled-body)
         ,@compiled-rest)))))

(define (emit-equality-check v1 v2 scope)
  (cond ((or (unbound-symbol? v1 scope)
             (unbound-symbol? v2 scope)
             (equal? '(fail) v1)
             (equal? '(fail) v2))
         `(scm.eq? ,(compile-expression v1 scope)
                   ,(compile-expression v2 scope)))
        ((or (string? v1) (string? v2))
         `(scm.equal? ,(compile-expression v1 scope)
                      ,(compile-expression v2 scope)))
        ((null? v1) `(scm.null? ,(compile-expression v2 scope)))
        ((null? v2) `(scm.null? ,(compile-expression v1 scope)))
        (else `(= ,(compile-expression v1 scope)
                  ,(compile-expression v2 scope)))))

(define binary-op-mappings
  '((+ . scm.+)
    (- . scm.-)
    (* . scm.*)
    (> . scm.>)
    (< . scm.<)
    (>= . scm.>=)
    (<= . scm.<=)
    (cons . scm.cons)
    (write-byte . scm.write-u8)))

(define unary-op-mappings
  '((number? . scm.number?)
    (string? . scm.string?)
    (cons? . scm.pair?)
    (absvector? . scm.vector?)
    (simple-error . scm.error)
    (hd . scm.car)
    (tl . scm.cdr)
    (read-byte . scm.read-u8)))

(define (binary-op-mapping op)
  (let ((res (assq op binary-op-mappings)))
    (and res (cdr res))))

(define (unary-op-mapping op)
  (let ((res (assq op unary-op-mappings)))
    (and res (cdr res))))

(define (emit-application op params scope)
  (let* ((arity (function-arity op))
         (partial-call? (not (or (= arity -1) (= arity (length params)))))
         (args (map (lambda (exp) (compile-expression exp scope))
                    params))
         (args-list (left-to-right `(scm.list ,@args))))
    (cond ((and (<= arity 0) (null? args))
           (cond ((pair? op) `(,(compile-expression op scope)))
                 ((unbound-symbol? op scope) `(,op))
                 (else `(,op))))
          (partial-call?
           (left-to-right (nest-call (nest-lambda op arity '()) args)))
          ((or (pair? op) (not (unbound-symbol? op scope)))
           (left-to-right
            (nest-call (compile-expression op scope) args)))
          (else
           (cond ((and (= arity 2) (binary-op-mapping op))
                  => (lambda (op) (left-to-right (cons op args))))
                 ((and (= arity 1) (unary-op-mapping op))
                  => (lambda (op) (left-to-right (cons op args))))
                 (else (left-to-right (cons op args))))))))

(define (nest-call op args)
  (if (null? args)
      op
      (nest-call (list op (car args)) (cdr args))))

(define (nest-lambda callable arity scope)
  (define (merge-args f arg)
    (if (pair? f)
        (append f (list arg))
        (list f arg)))

  (if (<= arity 0)
      (compile-expression callable scope)
      (let ((aname (gensym "Y")))
        `(scm.lambda (,aname)
           ,(nest-lambda (merge-args callable aname)
                         (- arity 1)
                         (cons aname scope))))))

;; Enforce left-to-right evaluation if needed
(define (left-to-right expr)
  (if (or (memq (car expr) '(trap-error set and or if freeze thaw
                             scm.and scm.if scm.or scm.cond scm.lambda
                             scm.let scm.letrec scm.let*))
          (< (length (filter pair? expr)) 2))
      expr
      `(scm.l2r ,expr ())))

(define (kl->scheme expr)
  (match expr
    (`(defun ,name ,args ,body)
     `(scm.begin
        (scm.register-function-arity (scm.quote ,name) ,(length args))
        (scm.define (,name ,@args) ,(compile-expression body args))
        (scm.quote ,name)))
    (else (compile-expression expr '()))))
