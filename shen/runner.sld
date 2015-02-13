(define-library (shen runner)
  (import (chibi)
          (shen))
  (begin
    (define (main arguments)
      (cond ((null? (cdr arguments)) (shen.shen))
            (else (for-each shen.load (cdr arguments)))))))

