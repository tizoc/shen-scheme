;; Copyright (c) 2012-2015 Bruno Deferrari.  All rights reserved.
;; BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(define-library (precompile)
  (import (shen reader)
          (shen primitives)
          (chibi match)
          (scheme base)
          (scheme read)
          (scheme process-context)
          (scheme file)
          (scheme write))

  (export compile)

  (begin
    (define (write-kl expr out)
      (cond ((pair? expr)
             (display "(" out)
             (for-each (lambda (expr)
                         (write-kl expr out)
                         (display " " out))
                       expr)
             (display ")" out))
            ((memq expr '(|{| |}| |;| |[| |]|))
             (display (string-append "|" (symbol->string expr) "|") out))
            (else (write expr out))))

    (define (dump-scm exprs out)
      (for-each (lambda (expr)
                  (write-kl expr out)
                  (newline out)
                  (newline out))
                exprs))

    (define *overwrites* '())

    (define (overwrite defun)
      (let ((name (cadr defun)))
        (set! *overwrites* (cons (cons name defun) *overwrites*))))

    (define (load-overwrites)
      (call-with-input-file "overwrites.scm"
        (lambda (in)
          (let loop ((defun (read in)))
            (if (eof-object? defun)
                'done
                (begin
                  (overwrite defun)
                  (loop (read in))))))))

    (define (kl->scheme-with-overwrites expr)
      (match expr
        (('defun name . rest) (let ((ow (assq name *overwrites*)))
                                (if ow
                                    (cdr ow)
                                    (kl->scheme expr))))
        (else (kl->scheme expr))))

    (define (compile-kl-file in out)
      (let read-loop ((res '()))
        (let ((exp (read-kl in)))
          (if (eof-object? exp)
              (dump-scm (map kl->scheme-with-overwrites (reverse res))
                        out)
              (read-loop (cons exp res))))))

    (define (compile)
      (load-overwrites)
      (compile-kl-file (current-input-port) (current-output-port)))

    ))
