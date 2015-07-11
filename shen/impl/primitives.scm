;; Copyright (c) 2012-2015 Bruno Deferrari.  All rights reserved.
;; BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

;; Utils

(define (call-with-output-string proc)
  (let ((out (open-output-string)))
    (proc out)
    (get-output-string out)))

;; Boolean Operators
;;

(define (assert-boolean value)
  (if (boolean? value)
      value
      (error "expected a boolean, got" value)))

;; Symbols
;;

(define (kl:intern name)
  (cond ((equal? name "true") #t)
        ((equal? name "false") #f)
        (else (string->symbol name))))

;; Strings
;;

(define (kl:str value)
  (call-with-output-string
   (lambda (o)
     (cond ((eq? value #t) (write 'true o))
           ((eq? value #f) (write 'false o))
           ((symbol? value)
            (display (symbol->string value) o))
           (else
            (write-simple value o))))))

;; Assignments
;;

(define *shen-globals* (make-hash-table))

(define (kl:set key val)
  (hash-table-set! *shen-globals* key val)
  val)

(define (kl:value key)
  (hash-table-ref *shen-globals*
                  key
                  (lambda () (error "variable has no value:" key))))

;; Error Handling
;;

(define (kl:error-to-string e)
  (call-with-output-string
   (lambda (out)
     (display (error-object-message e) out)
     (let ((irritants (error-object-irritants e)))
       (if (not (null? irritants))
           (begin
             (display ": " out)
             (write-simple (error-object-irritants e) out)))))))

;; Generic Functions
;;

(define (vector=? a b)
  (let ((len (vector-length a)))
    (and (= len (vector-length b))
         (do ((i 0 (+ i 1)))
             ((or (= i len)
                  (not (kl:= (vector-ref a i)
                             (vector-ref b i))))
              (= i len))))))

(define (kl:= a b)
  (cond ((eq? a b) #t) ;; fast path
        ((number? a) (and (number? b) (= a b)))
        ((pair? a)
         (and (pair? b)
              (kl:= (car a) (car b))
              (kl:= (cdr a) (cdr b))))
        ((string? a) (and (string? b) (string=? a b)))
        ((vector? a) (and (vector? b) (vector=? a b)))
        ;; the first eq? test already covers for null and symbols
        (else #f)))

(define *shen-environment* #f)

(define (set-shen-environment! env)
  (set! *shen-environment* env))

(define (eval-in-shen expr)
  (eval expr *shen-environment*))

(define (kl:eval-kl expr)
  (eval-in-shen (kl->scheme expr)))

;; Streams and I/O
;;

(define (full-path-for-file filename)
  (path-resolve filename
                (kl:value '*home-directory*)))

(define (kl:open filename direction)
  (let ((full-path (full-path-for-file filename)))
    (case direction
      ((in) (if (file-exists? full-path)
                (open-input-file full-path)
                (error "File does not exist" full-path)))
      ((out) (open-output-file full-path))
      (else (error "Invalid direction" direction)))))

(define (kl:close stream)
  (cond
   ((input-port? stream) (close-input-port stream))
   ((output-port? stream) (close-output-port stream))
   (else (error "invalid stream" stream))))

;; Time
;;

(define (kl:get-time sym)
  (case sym
    ;; TODO: run, date, more presicion
    ((real) (current-second))
    ((run) (current-second))
    (else (error "get-time does not understand the parameter" sym))))

;; Support

(define-syntax l2r
  (syntax-rules ()
    ((_ () ?expr) ?expr)
    ((_ (?op ?params ...) (?expr ...))
     (let ((f ?op))
       (l2r (?params ...) (?expr ... f))))))
