;; Copyright (c) 2012-2015 Bruno Deferrari.  All rights reserved.
;; BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(define (read-file-as-string filename)
  (call-with-input-file (full-path-for-file filename)
    port->string))

(define (read-file-as-bytelist filename)
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
  (set! shen-*system* (make-hash-table))

  (for-each
   (lambda (sym) (hash-table-set! shen-*system*
                                  (case sym
                                    ((#t) 'true)
                                    ((#f) 'false)
                                    (else sym)) #t))
   (kl:eval-kl
    '(get shen shen.external-symbols (value *property-vector*))))

  shen-*system*)

(define (shen-sysfunc? val)
  (let ((table (or shen-*system* (init-*system*))))
    (hash-table-ref/default table val #f)))
