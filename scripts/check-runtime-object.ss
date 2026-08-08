(import (chezscheme))

(define args (cdr (command-line)))

(unless (= (length args) 1)
  (display
    "usage: check-runtime-object.ss LIBRARY-DIRECTORY\n"
    (current-error-port))
  (exit 2))

(define root (car args))

(parameterize ([library-directories
                (cons (cons root root) (library-directories))])
  (let* ([runtime (environment '(shen-scheme runtime))]
         [run-shen (eval 'kl:shen-scheme.run-shen runtime)])
    (unless (procedure? run-shen)
      (error 'check-runtime-object
             "the composite runtime does not export shen-scheme.run-shen"))
    (printf "[OK]    composite runtime is importable\n")))
