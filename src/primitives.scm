;; Copyright (c) 2012-2018 Bruno Deferrari.  All rights reserved.
;; BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

;; Boolean Operators
;;

(define (kl:_scm.assert-boolean value)
  (if (boolean? value)
      value
      (raise-error '_scm.assert-boolean "expected a boolean in if/and/or expression, got" value)))

;; Symbols
;;

(define (kl:intern name)
  (cond ((equal? name "true") #t)
        ((equal? name "false") #f)
        (else (string->symbol name))))

;; Numbers
;;

(define (non-rational-/ x y)
  (let ((result (/ x y)))
    (if (integer? result)
        result
        (inexact result))))

;; Strings
;;

(define (kl:str value)
  (call-with-string-output-port
   (lambda (o)
     (cond ((eq? value #t) (write 'true o))
           ((eq? value #f) (write 'false o))
           ((symbol? value)
            (display (symbol->string value) o))
           (else
            (put-datum o value))))))

;; Assignments
;;

(define (kl:set var val)
  (shen-global-set! var val)
  val)

(define (kl:value var)
  (shen-global-get var
                   (lambda () (raise-error 'value "variable has no value" var))))

(define (value/or var default)
  (shen-global-get var default))

;; Error Handling
;;

(define kl:error-to-string error-message)

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

;; Eval
;;

(define (kl:eval-kl expr)
  (eval (kl:_scm.kl->scheme expr)))

;; Streams and I/O
;;

(define (kl:open filename direction)
  (let ((full-path (full-path-for-file filename)))
    (case direction
      ((in) (if (file-exists? full-path)
                (open-binary-input-file full-path)
                (raise-error 'open "File does not exist" full-path)))
      ((out) (open-binary-output-file full-path))
      (else (raise-error 'open "Invalid direction" direction)))))

(define (kl:close stream)
  (cond
   ((input-port? stream) (close-input-port stream))
   ((output-port? stream) (close-output-port stream))
   (else (raise-error 'close "invalid stream" stream))))

(define (kl:write-byte byte o)
  (write-byte byte o))

(define (kl:read-byte i)
  (read-byte i))

;; Time
;;

(define (kl:get-time sym)
  (case sym
    ((unix) (time->float (current-time 'time-utc)))
    ((real) (time->float (current-time 'time-monotonic)))
    ((run) (time->float (current-time 'time-process)))
    (else (raise-error 'get-time "invalid option" sym))))

