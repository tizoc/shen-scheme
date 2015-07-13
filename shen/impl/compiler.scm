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

(define *yields-boolean*
  '(or and
    kl:=
    null? string? vector? number? pair?
    < > >= <= = eq? equal?
    kl:element? kl:symbol? kl:not kl:variable? kl:boolean?
    kl:empty? kl:shen.pvar? kl:tuple?))

(define (yields-boolean? expr)
  (cond
   ((boolean? expr) #t)
   ((pair? expr)
    (or (memq (car expr) *yields-boolean*)
        (and (eq? 'l2r (car expr))
             (yields-boolean? (car (cdr expr))))))
   (else #f)))

(define (force-boolean expr)
  (if (yields-boolean? expr)
      expr
      `(assert-boolean ,expr)))

(define (compile-expression expr scope)
  (define (unbound? maybe-sym)
    (unbound-symbol? maybe-sym scope))

  (define (ce expr . extra-scope)
    (compile-expression expr (append extra-scope scope)))

  (match expr
    ((? null?) '(quote ()))
    ('true #t)
    ('false #f)
    ('|{| '(quote |{|))
    ('|}| '(quote |}|))
    ('|;| '(quote |;|))
    ((? unbound? sym) `(quote ,sym))
    (('let var value body) (emit-let var value body (cons var scope)))
    (('cond clauses ...) (emit-cond clauses scope))
    ;; Remove intermediary wrapper lambdas
    ;; FIXME: disabled for now, op may not be defined yet
    ;; (('lambda var ((? unbound? op) var)) op)
    (('lambda var body)
     `(lambda (,var) ,(ce body var)))
    (('and expr1 expr2)
     `(and ,(force-boolean (ce expr1))
           ,(force-boolean (ce expr2))))
    (('or expr1 expr2)
     `(or ,(force-boolean (ce expr1))
          ,(force-boolean (ce expr2))))
    (('if test then else)
     `(if ,(force-boolean (ce test))
          ,(ce then)
          ,(ce else)))
    (('trap-error expression ('lambda (v) body))
     `(guard (?exn
              (else (let ((,v ?exn))
                      ,(ce body v))))
        ,(ce expression)))
    (('trap-error expression handler)
     `(let ((?handler ,(ce handler)))
        (guard (?exn (else (?handler ?exn)))
          ,(ce expression))))
    (('do expr1 expr2) `(begin ,(ce expr1) ,(ce expr2)))
    (('freeze expr) `(lambda () ,(ce expr)))
    (('fail) '(quote shen.fail!))
    (('type x type) (ce x))
    (('scm. exp) (call-with-input-string exp read))
    (('= v1 v2) (emit-equality-check v1 v2 scope))
    (('/ a b) (left-to-right `(/ (inexact ,(ce a)) ,(ce b))))
    (('pos str n)
     `(string ,(left-to-right `(string-ref ,(ce str) ,(ce n)))))
    (('tlstr str) `(let ((_tmp ,(ce str)))
                     (substring _tmp 1 (string-length _tmp))))
    (('cn str1 str2)
     (left-to-right `(string-append ,(ce str1) ,(ce str2))))
    (('n->string n) `(string (integer->char ,(ce n))))
    (('string->n str) `(char->integer (string-ref ,(ce str) 0)))
    (('absvector n) `(make-vector ,(ce n) (quote shen.fail!)))
    (('<-address v n)
     (left-to-right `(vector-ref ,(ce v) ,(ce n))))
    (('address-> v n x)
     `(let ((_tmp ,(ce v)))
       ,(left-to-right `(vector-set! _tmp ,(ce n) ,(ce x)))
       _tmp))
    ((op params ...) (emit-application op params scope))
    (else expr)))

(define (emit-let var value body scope)
  `(let ((,var ,(compile-expression value scope)))
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
       `((,(force-boolean compiled-test) ,compiled-body)
         ,@compiled-rest)))))

(define (emit-equality-check v1 v2 scope)
  (cond ((or (unbound-symbol? v1 scope)
             (unbound-symbol? v2 scope)
             (equal? '(fail) v1)
             (equal? '(fail) v2))
         `(eq? ,(compile-expression v1 scope)
               ,(compile-expression v2 scope)))
        ((or (string? v1) (string? v2))
         `(equal? ,(compile-expression v1 scope)
                  ,(compile-expression v2 scope)))
        ((null? v1) `(null? ,(compile-expression v2 scope)))
        ((null? v2) `(null? ,(compile-expression v1 scope)))
        (else `(kl:= ,(compile-expression v1 scope)
                     ,(compile-expression v2 scope)))))

(define binary-op-mappings
  '((+ . +)
    (- . -)
    (* . *)
    (> . >)
    (< . <)
    (>= . >=)
    (<= . <=)
    (cons . cons)
    (write-byte . write-u8)))

(define unary-op-mappings
  '((number? . number?)
    (string? . string?)
    (cons? . pair?)
    (absvector? . vector?)
    (simple-error . error)
    (hd . car)
    (tl . cdr)
    (read-byte . read-u8)))

(define (binary-op-mapping op)
  (let ((res (assq op binary-op-mappings)))
    (and res (cdr res))))

(define (unary-op-mapping op)
  (let ((res (assq op unary-op-mappings)))
    (and res (cdr res))))

(define (prefix-op op)
  (let* ((sop (symbol->string op))
         (opl (string-length sop)))
    (if (and (> opl 4)
             (string=? "scm." (substring sop 0 4)))
        (string->symbol (substring sop 4 opl))
        (string->symbol (string-append "kl:" sop)))))

(define (emit-application op params scope)
  (let* ((arity (function-arity op))
         (partial-call? (not (or (= arity -1) (= arity (length params)))))
         (args (map (lambda (exp) (compile-expression exp scope))
                    params)))
    (cond ((and (<= arity 0) (null? args))
           (cond ((pair? op) `(,(compile-expression op scope)))
                 ((unbound-symbol? op scope) `(,(prefix-op op)))
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
                 (else
                  (let ((op (prefix-op op)))
                    (left-to-right (cons op args)))))))))

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
        `(lambda (,aname)
           ,(nest-lambda (merge-args callable aname)
                         (- arity 1)
                         (cons aname scope))))))

;; Enforce left-to-right evaluation if needed
(define (left-to-right expr)
  (if (or (memq (car expr)
                '(trap-error set and or if freeze thaw lambda
                  scm.and scm.if scm.or scm.cond scm.lambda
                  scm.let scm.letrec scm.let*))
          (< (length (filter pair? expr)) 2))
      expr
      `(l2r ,expr ())))

(define (kl->scheme expr)
  (match expr
    (`(defun ,name ,args ,body)
     `(begin
        (register-function-arity (quote ,name) ,(length args))
        (define (,(prefix-op name) ,@args) ,(compile-expression body args))
        (quote ,name)))
    (else (compile-expression expr '()))))
