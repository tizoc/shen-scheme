;; Copyright (c) 2012-2015 Bruno Deferrari.  All rights reserved.
;; BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(define-library (precompile)
  (import (shen reader)
          (shen primitives)
          (scheme base)
          (scheme read)
          (scheme process-context)
          (scheme file)
          (scheme write))

  (cond-expand
    (chibi (import (chibi match)))
    (gauche (import (util match))))

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
      (call-with-input-file "overwrites.kl"
        (lambda (in)
          (let loop ((defun (read-kl in)))
            (cond ((eof-object? defun) 'done)
                  (else
                   (and (list? defun) (overwrite defun))
                   (loop (read in))))))))

    (define (kl->scheme-with-overwrites expr)
      (match expr
        (('defun name . rest) (let* ((ow (assq name *overwrites*))
                                     (code (if ow (cdr ow) expr)))
                                (kl->scheme code)))
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
