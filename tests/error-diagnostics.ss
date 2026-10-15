(import (chezscheme) (shen-scheme runtime))

;; Run inside an initialized Shen/Scheme runtime, for example:
;; shen-scheme eval -q -e '(eval-kl [scm. "(load c#34;tests/error-diagnostics.ssc#34;)"])'

(define (check label actual)
  (unless actual
    (error 'error-diagnostics label))
  (printf "[OK]    ~a\n" label))

(define (contains? text part)
  (let ((limit (- (string-length text) (string-length part))))
    (let loop ((i 0))
      (and (<= i limit)
           (or (string=? part (substring text i (+ i (string-length part))))
               (loop (+ i 1)))))))

(define (caught thunk)
  (guard (e [else e])
    (thunk)
    (error 'error-diagnostics "expected an exception")))

(define (report)
  (call-with-string-output-port
    (lambda (out)
      (parameterize ([current-output-port out])
        (show-last-error)))))

(check "ordinary error messages retain their formatting"
  (equal? (kl:error-to-string
            (condition (make-message-condition "expected a value")
                       (make-irritants-condition '(42))))
          "expected a value: 42"))

(let* ((e (caught (lambda ()
                   (syntax-violation 'diagnostics "invalid syntax"
                     '(if #t unexpected) 'unexpected))))
       (message (kl:error-to-string e)))
  (check "syntax errors identify the bad subform and containing form"
    (and (contains? message "Bad syntax: unexpected")
         (contains? message "In: (if #t unexpected)"))))

(let* ((form '(if))
       (source (make-source-object
                 (source-file-descriptor "<diagnostics>" 0) 0 4 4 3))
       (e (caught (lambda () (eval (make-annotation form source form))))))
  (check "syntax errors include annotated source positions"
    (contains? (kl:error-to-string e) "At: <diagnostics>:4:3")))

(let* ((e (caught (lambda () (kl:eval-kl '(lambda 1 2)))))
       (message (kl:error-to-string e)))
  (check "real Chez syntax failures retain their syntax fields and continuation"
    (and (syntax-violation? e)
         (continuation-condition? e)
         (equal? (syntax->datum (syntax-violation-form e)) '(lambda (1) 2))
         (contains? message "In: (lambda (1) 2)")))
  (kl:declare '_scm.diagnostic-temporary '(number --> number))
  (let ((rethrown
          (caught (lambda ()
                    (kl:shen.unwind-types e
                      '(_scm.diagnostic-temporary (number --> number)))))))
    (check "typechecker cleanup re-raises the identical exception"
      (eq? e rethrown))
    (check "typechecker cleanup still removes temporary declarations"
      (null? (kl:assoc '_scm.diagnostic-temporary (kl:value 'shen.*sigf*))))))

(caught (lambda () (kl:eval-kl '(scm. "(kl:eval-kl '(lambda 1 2))"))))
(let ((saved-report (report)))
  (check "nested evaluation retains the innermost failed KL and Scheme"
    (and (contains? saved-report "KLambda:\n(lambda 1 2)")
         (contains? saved-report "Generated Scheme:\n(lambda (1) 2)")))
  (kl:eval-kl '(+ 1 2))
  (check "successful evaluations do not overwrite the saved failure"
    (string=? saved-report (report))))

(caught (lambda () (kl:eval-kl '(scm. "("))))
(check "failures during Scheme generation retain the input KL"
  (let ((text (report)))
    (and (contains? text "KLambda:")
         (contains? text "Scheme generation did not complete."))))

;; Fail while the typechecker executes a user datatype side condition. This
;; exercises check-eval-and-print -> unwind-types, not just an explicit rethrow.
(for-each kl:eval
  (kl:read-from-string
    "(define _scm.diagnostic-invalid-syntax -> (eval-kl [lambda 1 2]))
     (datatype _scm.diagnostic-type
       if (_scm.diagnostic-invalid-syntax)
       _______________________________
       diagnostic-value : _scm.diagnostic-type;)"))

(let ((e (caught (lambda ()
                   (kl:shen.check-eval-and-print
                     (kl:read-from-string
                       "diagnostic-value : _scm.diagnostic-type"))))))
  (check (format "a syntax exception survives the complete typechecker error path (got ~a)"
           (kl:error-to-string e))
    (and (syntax-violation? e)
         (continuation-condition? e)
         (equal? (syntax->datum (syntax-violation-form e)) '(lambda (1) 2))))
  (let ((message
          (call-with-string-output-port
            (lambda (out)
              (parameterize ([kl:global/*stoutput* out])
                (kl:set '*hush* #f)
                (kl:set 'shen.*show-exceptions* #f)
                (kl:shen.toplevel-display-exception e))))))
    (check "the REPL shows bad syntax with show-exceptions disabled"
      (contains? message "In: (lambda (1) 2)"))))

(check "the report command can be typechecked"
  (eq? (kl:shen.typecheck '(_scm.last-error) 'symbol) 'symbol))
(check "the debugger command can be typechecked"
  (eq? (kl:shen.typecheck '(_scm.debug-last-error) 'symbol) 'symbol))

(let ((saved-report (report)))
  (call-with-string-output-port
    (lambda (out)
      (parameterize ([current-output-port out])
        (check "the report command returns normally"
          (eq? (kl:_scm.last-error) 'ok)))))
  (check "inspecting the report preserves it"
    (string=? saved-report (report)))
  (let* ((old-debug-condition (debug-condition))
         (debug-output
           (call-with-string-output-port
             (lambda (out)
               (parameterize ([console-input-port
                               (open-string-input-port "i\nsf 2\nq\nq\n")]
                              [console-output-port out]
                              [current-output-port out])
                 (check "quitting the debugger returns to Shen"
                   (eq? (kl:_scm.debug-last-error) 'ok)))))))
    (check "the debugger can inspect the original raise continuation"
      (and (contains? debug-output "debug>")
           (not (contains? debug-output "the raise continuation is not available"))))
    (check "the debugger restores Chez's previous debug condition"
      (eq? old-debug-condition (debug-condition))))
  (check "the debugger preserves the saved diagnostic report"
    (string=? saved-report (report))))

(display "[OK]    error diagnostics\n")
