(import (chezscheme))

(define root "_build/build-library-tests")
(define public-source "_build/shen-library-test.ss")
(define public-object (string-append root "/shen.so"))
(define runtime-source "_build/shen-runtime-library-test.ss")
(define runtime-directory (string-append root "/shen-scheme"))
(define runtime-object (string-append root "/shen-scheme/runtime.so"))

(define (assert-procedure label value)
  (unless (procedure? value)
    (error 'check-build-library "~a is not a procedure" label))
  (printf "[OK]    ~a\n" label))

(define (assert-same label expected actual)
  (unless (eq? expected actual)
    (error 'check-build-library "~a does not preserve its implementation" label))
  (printf "[OK]    ~a\n" label))

(unless (file-exists? root)
  (mkdir root))
(unless (file-exists? runtime-directory)
  (mkdir runtime-directory))

(parameterize ([optimize-level 0]
               [compile-file-message #t]
               [library-directories
                (cons (cons root root) (library-directories))])
  (compile-file runtime-source runtime-object)
  (compile-file public-source public-object)
  (let ([shen (environment '(shen))])
    ;; The standalone Chez executable does not export the
    ;; get_shen_scheme_home_path C symbol used during initialization.  Importing
    ;; the complete generated library and checking the initializer binding
    ;; validates the wrapper without invoking that host-specific callback.
    (assert-procedure
     "public library exports initialize-shen"
     (eval 'initialize-shen shen))
    (assert-procedure
     "public library preserves shen.quiet-load"
     (eval 'kl:shen.quiet-load shen))
    (assert-procedure
     "public library preserves shen.run-shen"
     (eval 'kl:shen.run-shen shen))
    (assert-same
     "shen.quiet-load compatibility alias"
     (eval 'kl:shen.x.launcher.quiet-load shen)
     (eval 'kl:shen.quiet-load shen))
    (assert-same
     "shen.run-shen compatibility alias"
     (eval 'kl:shen-scheme.run-shen shen)
     (eval 'kl:shen.run-shen shen))))
