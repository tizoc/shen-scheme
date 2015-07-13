;; Copyright (c) 2012-2015 Bruno Deferrari.  All rights reserved.
;; BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(define-library (shen runner)
  (import (scheme base)
          (shen init))

  (cond-expand
   (gauche
    (import (gauche base))
    (begin
      ((setter port-buffering) (current-output-port) :none)
      ((setter port-buffering) (current-input-port) :none))))

  (begin
    (define (main arguments)
      (cond ((= 1 (length arguments)) (kl:shen.shen))
            (else (kl:shen.quiet-load (cadr arguments)))))))

