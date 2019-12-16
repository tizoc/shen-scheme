;; Copyright (c) 2012-2019 Bruno Deferrari.  All rights reserved.
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
        (else (or (and (> (string-length name) 1)
                      (char-numeric? (string-ref name 0))
                      (string->number name))
                  (string->symbol name)))))

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

;; If name contains the `kl:` prefix, remove it.
;; If it doesn't add an `scm.` prefix.
;; This is used to show the correct name on the REPL
;; when the result of evaluating a `defun` is printed.
(define (adjust-name name)
  (let ((len (string-length name)))
    (if (and (> len 3)
             (string=? "kl:" (substring name 0 3)))
        (substring name 3 len)
        (string-append "scm." name))))


;; (define (f ...) ...)
;; (define f ...)
(define (function-name expr)
  (let ((name (if (pair? (cadr expr))
                  (caadr expr)
                  (cadr expr))))
    (string->symbol (adjust-name (symbol->string name)))))

(define (kl:eval-kl expr)
  (let* ((scm-expr (kl:_scm.kl->scheme expr))
         (result (eval scm-expr)))
    (if (and (pair? scm-expr) (eq? (car scm-expr) 'define))
        (function-name scm-expr)
        result)))

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

;; Others

(define (make-equal-hashtable size)
  (make-hashtable equal-hash equal? size))

(define (value/or var default)
  (shen-global-get var default))

(define (get/or var prop dict default)
  (let* ((entry (hashtable-ref dict var '()))
         (res (assq prop entry)))
    (if (not res)
        (default)
        (cdr res))))

(define (<-address/or vector n default)
  (if (>= n (vector-length vector))
      (default)
      (vector-ref vector n)))

(define (<-vector/or vector n default)
  (if (or (zero? n) (>= n (vector-length vector)))
      (default)
      (let ((elt (vector-ref vector n)))
        (if (eq? elt 'shen.fail!)
            (default)
            elt))))

(define symbol-character?
  (let ((specials (string->list "=*/+-_?$!@~><&%{}:;`#'.")))
    (lambda (c)
      (or (char-alphabetic? c)
          (char-numeric? c)
          (not (eq? #f (memq c specials)))))))

(define (string-all? pred s)
  (let ((stop (string-length s)))
    (let loop ((i 0))
      (cond ((= i stop) #t)
            ((pred (string-ref s i)) (loop (+ i 1)))
            (else #f)))))

(define (analyse-symbol? s)
  (and (> (string-length s) 0)
       (not (char-numeric? (string-ref s 0)))
       (symbol-character? (string-ref s 0))
       (string-all? symbol-character? s)))
