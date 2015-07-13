;; Copyright (c) 2012-2015 Bruno Deferrari.  All rights reserved.
;; BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(define-library (shen support gauche srfi-69)

  (import
   (scheme base)
   (rename (gauche base)
           (hash gauche-hash)
           (make-hash-table make-hash-table)
           (hash-table-put! hash-table-set!)
           (hash-table-get hash-table-ref/default)))

  (export hash
          make-hash-table
          hash-table-set!
          hash-table-ref/default
          hash-table-ref)

  (begin
    (define (hash obj . bound)
      (let* ((bound (if (null? bound) #f (car bound)))
             (res (gauche-hash obj)))
        (if bound
            (modulo res bound)
            res)))

    (define *not-found* (list #f))

    (define (default-not-found-handler)
      (error "ERROR: hash-table-ref: key not found"))

    (define (hash-table-ref ht key . not-found-handler)
      (let* ((not-found-handler (if (null? not-found-handler)
                                    default-not-found-handler
                                    (car not-found-handler)))
             (res (hash-table-ref/default ht key *not-found*)))
        (if (eq? res *not-found*)
            (not-found-handler)
            res)))))
