;; Copyright (c) 2012-2015 Bruno Deferrari.  All rights reserved.
;; BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

;; Boolean Operators
;;

(define-syntax assert-boolean
  (syntax-rules ()
    ((_ ?value)
     (let ((value ?value))
       (if (boolean? value)
           value
           (error "expected a boolean, got" value))))))

(define-syntax kl:if
  (syntax-rules ()
    ((_ ?test ?then ?else)
     (if (assert-boolean ?test) ?then ?else))))

(define-syntax kl:and
  (syntax-rules ()
    ((_ ?value1)
     (let ((value1 ?value1))
       (lambda (value2) (kl:and value1 value2))))
    ((_ ?value1 ?value2)
     (and (assert-boolean ?value1) (assert-boolean ?value2)))))

(define-syntax kl:or
  (syntax-rules ()
    ((_ ?value1)
     (let ((value1 ?value1))
       (lambda (value2) (kl:or value1 value2))))
    ((_ ?value1 ?value2)
     (or (assert-boolean ?value1) (assert-boolean ?value2)))))

(define-syntax kl:cond
  (syntax-rules ()
    ((_) #f)
    ((_ (?test ?expr) ?clauses ...)
     (if (assert-boolean ?test)
         ?expr
         (kl:cond ?clauses ...)))))

;; Symbols
;;

(define (kl:intern name)
  (cond ((equal? name "true") #t)
        ((equal? name "false") #f)
        (else (string->symbol name))))

;; Strings
;;

(define (kl:pos str n) (string (string-ref str n)))

(define (kl:tlstr string) (substring string 1))

(define (kl:cn string1 string2)
  (string-append string1 string2))

(define (kl:str value)
  (call-with-output-string
   (lambda (o)
     (cond ((eq? value #t) (write 'true o))
           ((eq? value #f) (write 'false o))
           ((symbol? value)
            (display (symbol->string value) o))
           (else
            (write-simple value o))))))

(define kl:string? string?)

(define (kl:n->string n)
  (string (integer->char n)))

(define (kl:string->n str)
  (char->integer (string-ref str 0)))

;; Assignments
;;

(define *shen-globals* (make-hash-table eq?))

(define (kl:set key val)
  (hash-table-set! *shen-globals* key val)
  val)

(define (kl:value key)
  (hash-table-ref *shen-globals*
                  key
                  (lambda () (error "variable has no value:" key))))

;; Error Handling
;;

(define kl:simple-error error)

;; If handler is a lambda, translate it into a let expression
;; to avoid allocating closures unnecessarily.
;; Otherwise evaluate the expression in case it happens
;; to have side effects.
(define-syntax kl:trap-error
  (syntax-rules (lambda)
    ((_ ?expression (lambda (?v) ?body))
     (guard (exn (else (let ((?v exn) ?body))))
       ?expression))
    ((_ ?expression ?handler)
     (let ((handler ?handler))
       (guard (exn (else (handler exn)))
         ?expression)))))

(define (kl:error-to-string e)
  (call-with-output-string
   (lambda (out)
     (display (error-object-message e) out)
     (let ((irritants (error-object-irritants e)))
       (if (not (null? irritants))
           (begin
             (display ": " out)
             (write-simple (error-object-irritants e) out)))))))

;; Lists
;;

(define kl:cons cons)

(define kl:hd car)

(define kl:tl cdr)

(define kl:cons? pair?)

;; Generic Functions
;;

(define-syntax kl:defun
  (syntax-rules ()
    ((_ ?f (?args ...) ?expr)
     (begin
       (define (?f ?args ...)
         ?expr)
       (register-function '?f ?f)
       (register-function-arity '?f (length '(?args ...)))
       '?f))))

(define-syntax kl:lambda
  (syntax-rules ()
    ((_ ?arg ?expr) (lambda (?arg) ?expr))))

(define-syntax kl:let
  (syntax-rules ()
    ((_ ?name ?value ?expr)
     (let ((?name ?value)) ?expr))))

(define (vector=? a b)
  (let ((minlen (min (vector-length a) (vector-length b))))
    (and (= (vector-length a) (vector-length b))
         (do ((i 0 (+ i 1)))
             ((or (= i minlen)
                  (not (kl:= (vector-ref a i)
                             (vector-ref b i))))
              (= i minlen))))))

(define (kl:= a b)
  (cond ((eq? a b) #t) ;; fast path
        ((number? a) (and (number? b) (= a b)))
        ((pair? a)
         (and (pair? b)
              (kl:= (car a) (car b))
              (kl:= (cdr a) (cdr b))))
        ((string? a) (and (string? b) (string=? a b)))
        ;; if eq? was false none of these can result in #t
        ((or (null? a) (null? b) (symbol? a) (symbol? b)) #f)
        ((vector? a) (and (vector? b) (vector=? a b)))
        (else #f)))

(define *shen-environment* #f)

(define (scm.set-shen-environment! env)
  (set! *shen-environment* env))

(define (eval-in-shen expr)
  (eval expr *shen-environment*))

(define (kl:eval-kl expr)
  (eval-in-shen (kl->scheme expr)))

(define-syntax kl:freeze
  (syntax-rules ()
    ((_ ?expr) (lambda () ?expr))))

(define (kl:type val type)
  val) ;; FIXME: do something with type

;; Vectors
;;

(define (kl:absvector size)
  (make-vector size 'shen.fail!))

(define kl:<-address vector-ref)

(define (kl:address-> vec loc val)
  (vector-set! vec loc val)
  vec)

(define kl:absvector? vector?)

;; Streams and I/O
;;

(define kl:read-byte read-u8)
(define kl:write-byte write-u8)

(define (full-path-for-file filename)
  (make-path (kl:value '*home-directory*)
             filename))

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

;; Arithmetic
;;

(define (inexact-/ a b)
  (let ((res (/ a b)))
    (if (rational? res)
        (inexact res)
        res)))

(define kl:/ inexact-/)
(define (kl:+ a b) (+ a b))
(define (kl:- a b) (- a b))
(define (kl:* a b) (* a b))
(define (kl:> a b) (> a b))
(define (kl:< a b) (< a b))
(define (kl:>= a b) (>= a b))
(define (kl:<= a b) (<= a b))

(define kl:number? number?)

;; Support

(define-syntax scm.l2r
  (syntax-rules ()
    ((_ () ?expr) ?expr)
    ((_ (?op ?params ...) (?expr ...))
     (let ((f ?op))
       (scm.l2r (?params ...) (?expr ... f))))))

(define (scm.call-nested f args)
  (if (null? args)
      f
      (scm.call-nested (f (car args)) (cdr args))))

(define (scm.function-binding maybe-symbol)
  (if (symbol? maybe-symbol)
      (hash-table-ref *shen-functions* maybe-symbol
                      (lambda () (error "undefined function: "
                                        maybe-symbol)))
      maybe-symbol))

(define (scm.function f)
  (if (symbol? f)
      (kl:eval-kl (nest-lambda f (function-arity f)))
      f))

;; Function references by name

(define (or-function a b)
  (kl:or a b))

(define (and-function a b)
  (kl:and a b))

(map (lambda (name+ref) (apply register-function name+ref))
     `((intern ,kl:intern)
       (pos ,kl:pos)
       (tlstr ,kl:tlstr)
       (cn ,kl:cn)
       (str ,kl:str)
       (string? ,kl:string?)
       (string->n ,kl:string->n)
       (n->string ,kl:n->string)
       (set ,kl:set)
       (value ,kl:value)
       (simple-error ,kl:simple-error)
       (error-to-string ,kl:error-to-string)
       (cons ,kl:cons)
       (hd ,kl:hd)
       (tl ,kl:tl)
       (cons? ,kl:cons?)
       (= ,kl:=)
       (eval-kl ,kl:eval-kl)
       (type ,kl:type)
       (absvector ,kl:absvector)
       (<-address ,kl:<-address)
       (address-> ,kl:address->)
       (absvector? ,kl:absvector?)
       (read-byte ,kl:read-byte)
       (write-byte ,kl:write-byte)
       (open ,kl:open)
       (close ,kl:close)
       (get-time ,kl:get-time)
       (+ ,kl:+)
       (- ,kl:-)
       (* ,kl:*)
       (/ ,kl:/)
       (> ,kl:>)
       (< ,kl:<)
       (>= ,kl:>=)
       (<= ,kl:<=)
       (or ,or-function)
       (and ,and-function)
       (number? ,kl:number?)))
