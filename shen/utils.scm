(define *shen-environment* #f)

(define ($$set-shen-environment! env)
  (set! *shen-environment* env))

(define-syntax assert-boolean
  (syntax-rules ()
    ((_ ?value)
     (let ((value ?value))
       (if (boolean? value)
           value
           (error "expected a boolean, got" value))))))

(define (full-path-for-file filename)
  (make-path (kl:value '*home-directory*)
             filename))
