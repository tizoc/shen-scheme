;; Copyright (c) 2012-2015 Bruno Deferrari.  All rights reserved.
;; BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(define (full-path-for-file filename)
  (make-path (kl:value '*home-directory*)
             filename))

(define (scm.read-file-as-string filename)
  (call-with-input-file (full-path-for-file filename)
    port->string))

(define (scm.read-file-as-bytelist filename)
  (call-with-input-file (full-path-for-file filename)
    (lambda (in)
      (let ((bytes (read-bytevector 1000000 in)))
        (let loop ((position (- (bytevector-length bytes) 1))
                   (result '()))
          (if (< position 0)
              result
              (loop (- position 1)
                    (cons (bytevector-u8-ref bytes position) result))))))))

(define shen-*system* #f)

(define (init-*system*)
  (set! shen-*system* (make-hash-table eq?))

  (for-each
   (lambda (sym) (hash-table-set! shen-*system*
                                  (case sym
                                    ((#t) 'true)
                                    ((#f) 'false)
                                    (else sym)) #t))
   ((scm.function-binding 'get)
    'shen 'shen.external-symbols (kl:value '*property-vector*)))

  shen-*system*)

(define (scm.shen-sysfunc? val)
  (let ((table (or shen-*system* (init-*system*))))
    (hash-table-ref/default table val #f)))

(define (scm.shen-grammar_symbol? val)
  (and (symbol? val)
       (let* ((s (symbol->string val))
              (end (string-cursor-end s))
              (start (string-cursor-start s))
              (c (string-cursor-prev s end)))
         (and (equal? #\> (string-cursor-ref s c))
              (let loop ((c (string-cursor-prev s c)))
                (if (or (string-cursor<? c start)
                        (equal? #\. (string-cursor-ref s c)))
                    (equal? #\< (string-cursor-ref s (string-cursor-next s c)))
                    (loop (string-cursor-prev s c))))))))
