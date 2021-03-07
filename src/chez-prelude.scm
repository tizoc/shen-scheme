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

(define (error-message e)
  (let* ((msg (condition-message e))
         (irritants (if (irritants-condition? e) (condition-irritants e) '())))
    (cond ((format-condition? e)
           (apply format msg (map kl-var-clean irritants)))
          ((null? irritants) msg)
          (else (format "~a: ~{~s~}" msg irritants)))))

(define (error-location e)
  (if (who-condition? e)
      (condition-who e)
      '*unknown-location*))

(define (full-path-for-file filename)
  (if (path-absolute? filename)
      filename
      (string-append (current-directory)
                     (string (directory-separator))
                     filename)))

(define (open-binary-input-file filename)
  (open-file-input-port filename))

(define (open-binary-output-file filename)
  (open-file-output-port filename (file-options no-fail)))

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

(define (read-file-as-string filename)
  (call-with-input-file (full-path-for-file filename)
    (lambda (in)
      (let ((s (get-string-all in)))
        (if (eof-object? s)
            ""
            s)))))

(define (read-file-as-bytelist filename)
  (let* ((in (open-file-input-port (full-path-for-file filename)))
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
              (loop (fx+ i 1) (f k v acc))))))))
