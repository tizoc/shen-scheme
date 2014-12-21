(define ($$read-file-as-string filename)
  (call-with-input-file (full-path-for-file filename)
    port->string))

(define ($$read-file-as-bytelist filename)
  (call-with-input-file (full-path-for-file filename)
    (lambda (in)
      (let ((bytes (read-bytevector 1000000 in)))
        (let loop ((position (- (bytevector-length bytes) 1))
                   (result '()))
          (if (< position 0)
              result
              (loop (- position 1)
                    (cons (bytevector-u8-ref bytes position) result))))))))

(define shen-*system* (make-hash-table eq?))

(define ($$init-*system*)
  (for-each
   (lambda (sym) (hash-table-set! shen-*system*
                                  (case sym
                                    ((#t) 'true)
                                    ((#f) 'false)
                                    (else sym)) #t))
   (($$function-binding 'get)
    'shen 'shen.external-symbols (kl:value '*property-vector*))))

(define ($$shen-sysfunc? val)
  (hash-table-ref/default shen-*system* val #f))

(define ($$shen-walk func val)
  (if (pair? val)
      (func (map (lambda (subexp) ($$shen-walk func subexp)) val))
      (func val)))

(define (compose funcs value)
  (if (null? funcs)
      value
      (compose (cdr funcs) ((car funcs) value))))

(define ($$macroexpand expr)
  (define macros (map $$function-binding (kl:value '*macros*)))

  (define (expand expr)
    (let ((transformed (compose macros expr)))
      (if (or (eq? expr transformed)
              (and (pair? expr) (eq? (car expr) '$native)))
          expr
          ($$shen-walk expand transformed))))

  (expand expr))
