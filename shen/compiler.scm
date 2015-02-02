;; Copyright (c) 2012-2015 Bruno Deferrari.  All rights reserved.
;; BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

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
    (('lambda var ((? unbound? op) var)) op)
    (('lambda var body)
     `(lambda ,var ,(compile-expression body (cons var scope))))
    (('do expr1 expr2)
     `(scm.begin ,(compile-expression expr1 scope) ,(compile-expression expr2 scope)))
    (('fail) '(scm.quote shen.fail!))
    (('$native exp) exp)
    (('$native . exps) `(scm.begin ,@exps))
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
                 (else `((scm.function-binding ,op)))))
          (partial-call?
           `(scm.call-nested ,(nest-lambda op arity) ,args-list))
          ((or (pair? op) (not (unbound-symbol? op scope)))
           (left-to-right
            `(scm.call-nested (scm.function ,(compile-expression op scope)) ,args-list)))
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
        `(lambda ,aname
           ,(nest-lambda (merge-args callable aname) (- arity 1))))))

;; Enforce left-to-right evaluation if needed
(define (left-to-right expr)
  (if (or (memq (car expr) '(trap-error set and or if freeze thaw))
          (< (length (filter pair? expr)) 2))
      expr
      `(scm.l2r ,expr ())))

(define (kl->scheme expr)
  (match expr
    (`(defun ,name ,args ,body)
     ;; pre-register arity in case the function is recursive
     ;; and partially-calls itself
     (register-function-arity name (length args))
     `(defun ,name ,args
        ,(compile-expression body args)))
    (else (compile-expression expr '()))))
