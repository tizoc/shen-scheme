;; Copyright (c) 2012-2015 Bruno Deferrari.  All rights reserved.
;; BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(define-library (shen runner)
  (import (scheme base)
          (shen init))
  (begin
    (define (main arguments)
      (cond ((= 1 (length arguments)) (shen.shen))
            (else (shen.quiet-load (cadr arguments)))))))

