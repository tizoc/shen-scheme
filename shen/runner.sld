(define-library (shen runner)
  (import (chibi)
          (shen init))
  (begin
    (define (main arguments)
      ;; FIXME: it is unclear how arguments really work here
      ;; sometimes argv[0] gets passed in, sometimes it doesn't
      (cond ((or (null? arguments) (equal? arguments '("chibi-scheme")))
             (shen.shen))
            (else (for-each shen.load arguments))))))

