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
     `(scm.lambda (,var) ,(compile-expression body (cons var scope))))
    (('and expr1 expr2)
     `(scm.and (scm.assert-boolean ,(compile-expression expr1 scope))
               (scm.assert-boolean ,(compile-expression expr2 scope))))
    (('and expr)
     `(scm.let ((?tmp ,(compile-expression expr scope)))
        (scm.lambda (?arg) (scm.and (scm.assert-boolean ?tmp)
                                    (scm.assert-boolean ?arg)))))
    (('or expr1 expr2)
     `(scm.or (scm.assert-boolean ,(compile-expression expr1 scope))
              (scm.assert-boolean ,(compile-expression expr2 scope))))
    (('or expr)
     `(scm.let ((?tmp ,(compile-expression expr scope)))
        (scm.lambda (?arg) (scm.or (scm.assert-boolean ?tmp)
                                   (scm.assert-boolean ?arg)))))
    (('if test then else)
     `(scm.if (scm.assert-boolean ,(compile-expression test scope))
              ,(compile-expression then scope)
              ,(compile-expression else scope)))
    (('trap-error expression ('lambda (v) body))
     `(scm.guard (?exn
                  (scm.else (scm.let ((,v ?exn))
                              ,(compile-expression body (cons v scope)))))
        ,(compile-expression expression scope)))
    (('trap-error expression handler)
     `(scm.let ((?handler ,(compile-expression handler scope)))
        (scm.guard (?exn (scm.else (?handler ?exn)))
          ,(compile-expression expression scope))))
    (('do expr1 expr2)
     `(scm.begin ,(compile-expression expr1 scope)
                 ,(compile-expression expr2 scope)))
    (('freeze expr) `(scm.lambda () ,(compile-expression expr scope)))
    (('fail) '(scm.quote shen.fail!))
    (('scm. exp) (call-with-input-string exp read))
    (('= v1 v2) (emit-equality-check v1 v2 scope))
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

(define (emit-application op params scope)
  (let* ((arity (function-arity op))
         (partial-call? (not (or (= arity -1) (= arity (length params)))))
         (args (map (lambda (exp) (compile-expression exp scope))
                    params))
         (args-list (left-to-right `(scm.list ,@args))))
    (cond ((null? args)
           (cond ((pair? op) `(,(compile-expression op scope)))
                 ((unbound-symbol? op scope) `(,op))
                 (else `(,op))))
          (partial-call?
           `(scm.call-nested ,(nest-lambda op arity) ,args-list))
          ((or (pair? op) (not (unbound-symbol? op scope)))
           (left-to-right
            `(scm.call-nested ,(compile-expression op scope) ,args-list)))
          (else
           (left-to-right (cons op args))))))

(define (nest-lambda callable arity)
  (define (merge-args f arg)
    (if (pair? f)
        (append f (list arg))
        (list f arg)))

  (if (<= arity 0)
      callable
      (let ((aname (gensym "Y")))
        `(scm.lambda (,aname)
           ,(nest-lambda (merge-args callable aname) (- arity 1))))))

;; Enforce left-to-right evaluation if needed
(define (left-to-right expr)
  (if (or (memq (car expr) '(trap-error set and or if freeze thaw
                             scm.and scm.if scm.or scm.cond))
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
