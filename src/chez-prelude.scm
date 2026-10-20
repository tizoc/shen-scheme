;; Copyright (c) 2012-2021 Bruno Deferrari.  All rights reserved.
;; BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(define-syntax raise-error
  (syntax-rules ()
    ((_ location message obj ...)
     (error location message obj ...))))

(define *shen-globals* (make-hashtable symbol-hash eq?))

(define (shen-global-parameter-set! var parameter)
  (symbol-hashtable-set! *shen-globals* var parameter))

(define (shen-global-set! var value)
  (let ((cell (symbol-hashtable-cell *shen-globals* var #f)))
    (if (cdr cell)
        ((cdr cell) value)
        (set-cdr! cell (make-parameter value)))))

(define *hash-table-default* (string-append "_" "-"))

(define (shen-global-get var default)
  (let ((res (symbol-hashtable-ref *shen-globals* var *hash-table-default*)))
    (if (eq? res *hash-table-default*)
        (default var)
        (res))))

(define (kl-var-clean sym)
  (if (symbol? sym)
      (let* ((str (symbol->string sym))
             (len (string-length str)))
        (if (and (> len 3)
                 (string=? "kl:" (substring str 0 3)))
         (string->symbol (substring str 3 len))
         sym))
      sym))

(define (syntax-error-details e)
  (if (syntax-violation? e)
      (let* ((form (syntax-violation-form e))
             (subform (syntax-violation-subform e))
             (annotation (or (syntax->annotation subform)
                             (syntax->annotation form))))
        (call-with-string-output-port
          (lambda (out)
            (parameterize ([print-level 6] [print-length 12])
              (when subform
                (fprintf out "\n  Bad syntax: ~s" (syntax->datum subform)))
              (fprintf out "\n  In: ~s" (syntax->datum form)))
            (when annotation
              (let ((src (annotation-source annotation)))
                (call-with-values
                  (lambda () (locate-source-object-source src #t #f))
                  (case-lambda
                    [() (fprintf out "\n  At: ~a, char ~a"
                          (source-file-descriptor-path (source-object-sfd src))
                          (source-object-bfp src))]
                    [(path line column)
                     (fprintf out "\n  At: ~a:~a:~a" path line column)])))))))
      ""))

(define (error-message e)
  (if (message-condition? e)
      (let* ((msg (condition-message e))
             (irritants (if (irritants-condition? e) (condition-irritants e) '()))
             (message (cond ((format-condition? e)
                             (apply format msg (map kl-var-clean irritants)))
                            ((null? irritants) msg)
                            (else (format "~a: ~{~s~}" msg irritants)))))
        (string-append message (syntax-error-details e)))
      (call-with-string-output-port
        (lambda (out) (display-condition e out)))))

(define (error-location e)
  (if (who-condition? e)
      (condition-who e)
      '*unknown-location*))

;; Keep the original condition, including its raise continuation. The optional
;; context is the innermost failing eval-kl, not the most recent successful eval.
(define last-exception (make-parameter #f))

(define (remember-exception! e)
  (let ((saved (last-exception)))
    (unless (and saved (eq? e (vector-ref saved 0)))
      (last-exception (vector e #f))))
  e)

(define (remember-eval-exception! e kl scm translated?)
  (remember-exception! e)
  (let ((saved (last-exception)))
    (unless (vector-ref saved 1)
      (vector-set! saved 1 (vector kl scm translated?)))))

(define (show-last-error)
  (let ((saved (last-exception))
        (out (current-output-port)))
    (if saved
        (let ((e (vector-ref saved 0))
              (context (vector-ref saved 1)))
          (parameterize ([print-level #f] [print-length #f])
            (display-condition e out)
            (newline out)
            (when (syntax-violation? e)
              (let ((subform (syntax-violation-subform e)))
                (when subform
                  (display "Bad syntax:\n" out)
                  (pretty-print (syntax->datum subform) out)))
              (display "Containing form:\n" out)
              (pretty-print (syntax->datum (syntax-violation-form e)) out))
            (when context
              (display "KLambda:\n" out)
              (pretty-print (vector-ref context 0) out)
              (if (vector-ref context 2)
                  (begin
                    (display "Generated Scheme:\n" out)
                    (pretty-print (vector-ref context 1) out))
                  (display "Scheme generation did not complete.\n" out))))
          (flush-output-port out)
          'ok)
        (begin
          (display "No exception has been saved.\n" out)
          (flush-output-port out)
          'no-error))))

(define (debug-last-error)
  (let ((saved (last-exception)))
    (if saved
        ;; Chez's debugger resets when it exits. Return to the Shen caller
        ;; instead of resetting out of the application's scheme-start handler.
        (call/cc
          (lambda (return)
            (parameterize ([debug-condition (vector-ref saved 0)]
                           [reset-handler (lambda () (return 'ok))])
              (debug)
              'ok)))
        (show-last-error))))

(define (full-path-for-file filename)
  (if (path-absolute? filename)
      filename
      (string-append (unicode->shen-string (current-directory))
                     (string (directory-separator))
                     filename)))

(define (shen-scheme-file-exists? filename)
  (file-exists? (shen-string->unicode filename)))

(define (open-binary-input-file filename)
  (open-file-input-port (shen-string->unicode filename)))

(define (open-binary-output-file filename)
  (open-file-output-port (shen-string->unicode filename) (file-options no-fail)))

(define (time->float t)
  (+ (time-second t)
     (/ (time-nanosecond t) 1e+9)))

(define (should-flush? p)
  (let ((name (port-name p)))
    (or (equal? name "stdout") (equal? name "stderr"))))

(define (write-byte byte o)
  (put-u8 o byte)
  (and (should-flush? o)
       (flush-output-port o))
  byte)

(define (read-byte i)
  (let ((byte (get-u8 i)))
    (if (eof-object? byte)
        -1
        byte)))

;; The kernel represents each input byte as a character with the same code.
;; Latin-1 preserves that mapping without UTF-8 or newline conversion.
(define shen-byte-transcoder
  (make-transcoder (latin-1-codec) (eol-style none) (error-handling-mode raise)))

(define (shen-string->bytevector s)
  (string->bytevector s shen-byte-transcoder))

;; Shen strings contain bytes; Chez strings at the OS boundary contain Unicode.
;; Conversions are explicit: paths remain byte strings until the filesystem call.
(define shen-utf8-transcoder
  (make-transcoder (utf-8-codec) (eol-style none) (error-handling-mode raise)))

(define (unicode->shen-string s)
  (bytevector->string (string->utf8 s) shen-byte-transcoder))

(define (shen-string->unicode s)
  ;; Prefix a byte so the UTF-8 codec preserves a leading U+FEFF as data.
  (let ((text (bytevector->string
               (shen-string->bytevector (string-append "." s)) shen-utf8-transcoder)))
    (substring text 1 (string-length text))))

(define (read-file-as-string filename)
  (call-with-port
    (transcoded-port (open-binary-input-file (full-path-for-file filename))
                     shen-byte-transcoder)
    (lambda (in)
      (let ((s (get-string-all in)))
        (if (eof-object? s)
            ""
            s)))))

(define (read-file-as-bytelist filename)
  (let* ((in (open-binary-input-file (full-path-for-file filename)))
         (bytes (get-bytevector-all in)))
    (close-input-port in)
    (if (eof-object? bytes)
        '()
        (bytevector->u8-list bytes))))

(define (hashtable-fold ht f init)
  (let-values (((keys values) (hashtable-entries ht)))
    (let ((limit (vector-length keys)))
      (let loop ((i 0)
                 (acc init))
        (if (fx=? i limit)
            acc
            (let ((k (vector-ref keys i))
                  (v (vector-ref values i)))
              (loop (fx+ i 1) (((f k) v) acc))))))))
