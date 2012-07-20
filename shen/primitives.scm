;; Utils
;;

(define *shen-environment* #f)
(define *tracing-enabled* #f)

(define ($$set-shen-environment! env)
  (set! *shen-environment* env))

(define ($$toggle-tracing!)
  (set! *tracing-enabled* (not *tracing-enabled*))
  *tracing-enabled*)

(define partial
  (case-lambda
    ((proc) (lambda args (apply proc args)))
    ((proc arg1) (lambda args (apply proc arg1 args)))
    ((proc arg1 arg2) (lambda args (apply proc arg1 arg2 args)))
    ((proc arg1 arg2 arg3) (lambda args (apply proc arg1 arg2 arg3 args)))
    ((proc arg1 arg2 arg3 . more-args)
     (lambda args (apply proc arg1 arg2 arg3 (append more-args args))))))

(define-syntax assert-boolean
  (syntax-rules ()
    ((_ ?value)
     (let ((value ?value))
       (if (boolean? value)
           value
           (error "expected a boolean, got" value))))))

(define (full-path-for-file filename)
  (make-path (kl:value '*home-directory*)
             filename))

(define symbol-character-translations
  '((#\# . "$sharp$")
    (#\{ . "$openbrace$")
    (#\} . "$closebrace$")
    (#\; . "$semicolon$")
    (#\` . "$backquote$")
    (#\' . "$quote$")))

;; Boolean Operators
;;

(define-syntax kl:if
  (syntax-rules ()
    ((_ ?test)
     (let ((test ?test))
       (case-lambda
         ((then else)
          (kl:if test then else))
         ((then)
          (kl:if test then)))))
    ((_ ?test ?then)
     (partial (kl:if ?test) ?then))
    ((_ ?test ?then ?else)
     (if (assert-boolean ?test) ?then ?else))))

(define-syntax kl:and
  (syntax-rules ()
    ((_ ?value1)
     (let ((value1 ?value1))
       (lambda (value2) (kl:and value1 value2))))
    ((_ ?value1 ?value2)
     (and (assert-boolean ?value1) (assert-boolean ?value2)))
    ((_ ?value1 ?value2 ?value3 ?more ...)
     (let ((value1 ?value1))
       (and (assert-boolean value1)
            (kl:and ?value2 ?value3 ?more ...))))))

(define-syntax kl:or
  (syntax-rules ()
    ((_ ?value1)
     (let ((value1 ?value1))
       (lambda (value2) (kl:or value1 value2))))
    ((_ ?value1 ?value2)
     (or (assert-boolean ?value1) (assert-boolean ?value2)))
    ((_ ?value1 ?value2 ?value3 ?more ...)
     (let ((value1 ?value1))
       (or (assert-boolean value1)
           (kl:or ?value2 ?value3 ?more ...))))))

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
  (string->symbol (safe-symbol-name name)))

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

(define *shen-globals* (make-hash-table))

(define kl:set
  (case-lambda
    ((key) (partial kl:set key))
    ((key val)
     (hash-table-set! *shen-globals* key val)
     val)))

(define (kl:value key)
  (hash-table-ref *shen-globals*
                  key
                  (lambda () (error "variable has no value:" key))))

;; Error Handling
;;

(define kl:simple-error error)

(define-syntax kl:trap-error
  (syntax-rules ()
    ((_ ?expression ?handler)
     (guard (exn (else (?handler exn)))
       ?expression))))

(define (kl:error-to-string e)
  (call-with-output-string
   (lambda (out)
     (display (error-object-message e) out)
     (display ": " out)
     (write-simple (error-object-irritants e) out))))

;; Lists
;;

(define kl:cons
  (case-lambda
    ((x) (partial cons x))
    ((x y) (cons x y))))

(define (kl:hd pair)
  (if (null? pair)
      pair
      (car pair)))

(define (kl:tl pair)
  (if (null? pair)
      pair
      (cdr pair)))

(define kl:cons? pair?)

;; Generic Functions
;;

;; curried lambda
(define-syntax curried
  (syntax-rules ()
    ((_ () ?body)
     (lambda () ?body))
    ((_ (?arg) ?body)
     (letrec
         ((partial-application
           (case-lambda
             (() partial-application)
             ((?arg) ?body))))
       partial-application))
    ((_ (?arg ?last) ?body)
     (letrec
         ((partial-application
           (case-lambda
             (() partial-application)
             ((?arg) (curried (?last) (partial-application ?arg ?last)))
             ((?arg ?last) ?body))))
       partial-application))
    ((_ (?arg ?args ... ?last) ?body)
     (letrec
         ((max-args (length '(?arg ?args ... ?last)))
          (partial-application
           (case-lambda
             ((?arg ?args ... ?last) ?body)
             ((?arg ?args ...)
              (curried (?last) (partial-application ?arg ?args ... ?last)))
             (() partial-application)
             (args
              (if (> (length args) max-args)
                  (error "Too many args" (length args))
                  (lambda more-args
                    (let ((all-args (append args more-args)))
                      (if (> (length all-args) max-args)
                          (error "Too many args" (length all-args))
                          (apply partial-application all-args)))))))))
       partial-application))))

;; curried defines
(define-syntax define-curried
  (syntax-rules ()
    ((_ (?name ?args ...) ?body)
     (define ?name (curried (?args ...) ?body)))
    ((_ ?name ?body)
     (define ?name ?body))))

(define-syntax kl:defun
  (syntax-rules ()
    ((_ ?f (?args ...) ?expr)
     (begin
       (define-curried (?f ?args ...) ?expr)
       '?f))))

(define-syntax kl:lambda
  (syntax-rules ()
    ((_ ?arg ?expr) (lambda (?arg) ?expr))))

(define-syntax kl:let
  (syntax-rules ()
    ((_ ?name ?value ?expr)
     (let ((?name ?value)) ?expr))))

(define-curried (kl:= a b)
  (cond ((and (number? a) (number? b))
         (= a b))        
        ((null? a)
         (null? b))
        ((and (symbol? a) (symbol? b))
         (eq? a b))
        ((and (pair? a) (pair? b))
         (and (kl:= (car a) (car b))
              (kl:= (cdr a) (cdr b))))
        ((and (string? a) (string? b))
         (string=? a b))
        ((and (vector? a) (vector? b)) ;; TODO: avoid intermediate list
         (kl:= (vector->list a) (vector->list b)))
        (else (equal? a b))))

(define ($$eval-in-shen expr)
  (eval expr *shen-environment*))

(define (kl:eval-kl expr)
  ($$eval-in-shen (kl->scheme expr)))

(define-curried (or-function a b)
  (kl:or a b))

(define-curried (and-function a b)
  (kl:and a b))

(define ($$function-binding maybe-symbol)
  (if (symbol? maybe-symbol)
      (case maybe-symbol
        ((or) or-function)
        ((and) and-function)
        (else (eval maybe-symbol *shen-environment*)))
      maybe-symbol))

(define-syntax kl:freeze
  (syntax-rules ()
    ((_ ?expr) (lambda () ?expr))))

(define (kl:type val type)
  val) ;; FIXME: do something with type

;; Vectors
;;

(define (kl:absvector size)
  (make-vector size 'fail!))

(define kl:<-address vector-ref)

(define (kl:address-> vec loc val)
  (vector-set! vec loc val)
  vec)

(define kl:absvector? vector?)

;; Streams and I/O
;;

(define kl:pr
  (case-lambda
    ((string) (kl:pr string (kl:value '*stoutput*)))
    ((string out)
     ;; Override out because Shen sends *stinput*
     (let ((out (if (eq? out (kl:value '*stinput*))
                    (kl:value '*stoutput*)
                    out)))
       (display string out)
       (flush-output-port out)
       string))))

(define kl:read-byte read-u8)

(define (kl:open type filename direction)
  (case type
    ((file)
     (let ((full-path (full-path-for-file filename)))
       (if (file-exists? full-path)
           (case direction
             ((in) (open-input-file full-path))
             ((out) (open-output-file full-path))
             (else (error "Invalid direction" direction)))
           (error "File does not exist" full-path))))
    (else (error "Invalid stream type" type))))

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

(define-syntax define-partial-op
  (syntax-rules ()
    ((_ ?alias ?op)
     (define ?alias
       (case-lambda
         (() ?alias)
         ((x) (partial ?alias x))
         ((x y) (?op x y)))))))

(define-partial-op kl:+ +)
(define-partial-op kl:- -)
(define-partial-op kl:* *)
(define-partial-op kl:/ inexact-/)
(define-partial-op kl:> >)
(define-partial-op kl:< <)
(define-partial-op kl:>= >=)
(define-partial-op kl:<= <=)

(define kl:number? number?)

;; Kl to Scheme translator
;;

(define (quote-let-vars vars scope)
  (if (null? vars)
      '()
      (let ((var (car vars))
            (value (cadr vars))
            (rest (cddr vars)))
        (cons var (cons (quote-expression value scope)
                        (quote-let-vars rest (cons var scope)))))))

(define (quote-cond-clauses clauses scope)
  (if (null? clauses)
      '()
      (let ((test (caar clauses))
            (body (car (cdar clauses)))
            (rest (cdr clauses)))
        (cons (list (quote-expression test scope)
                    (quote-expression body scope))
              (quote-cond-clauses rest scope)))))

(define (unbound-symbol? maybe-sym scope)
  (and (symbol? maybe-sym)
       (not (memq maybe-sym scope))))

(define (quote-expression expr scope)
  (define (unbound-in-current-scope? maybe-sym)
    (unbound-symbol? maybe-sym scope))

  (match expr
    ((? null?) '($$quote ()))
    ('true #t)
    ('false #f)
    ('|{| '($$quote |{|))
    ('|}| '($$quote |}|))
    ('|;| '($$quote |;|))
    ((? hazard-symbol? sym) (safe-symbol sym))
    ((? unbound-in-current-scope? sym) `($$quote ,(safe-symbol sym)))
    (('let var value body)
     `(let ,var ,(quote-expression value scope)
        ,(quote-expression body (cons var scope))))
    (('cond clauses ...)
     `(cond ,@(quote-cond-clauses clauses scope)))
    (('lambda var body)
     `(lambda ,var ,(quote-expression body (cons var scope))))
    (('do expr1 expr2)
     `($$begin ,(quote-expression expr1 scope) ,(quote-expression expr2 scope)))
    (('map 'shen-linearise exp) ;; Workaround for map with symbol in core.kl
     `(map shen-linearise ,exp))
    (`(defun ,name ,args ,body)
     (let ((name (safe-symbol name)))
       `($$eval-in-shen
         ($$quote
          (defun ,name ,(map safe-symbol args)
            ,(quote-expression body args))))))
    ((op param ...)
     (left-to-right
      (cons (function-binding op scope)
            (map (lambda (exp) (quote-expression exp scope))
                 param))))
    (else expr)))

(define (function-binding expr scope)
  (cond
   ((pair? expr) `($$function-binding ,(quote-expression expr scope)))
   ((unbound-symbol? expr scope) (safe-symbol expr))
   (else `($$function-binding ,(safe-symbol expr)))))

;; Enforce left-to-right evaluation if needed
(define (left-to-right expr)
  (if (or (memq (car expr) '(trap-error set and or if))
          (< (length (filter pair? expr)) 2))
      expr
      `($$l2r ,expr ())))

(define-syntax $$l2r
  (syntax-rules ()
    ((_ () ?expr) ?expr)
    ((_ (?op ?params ...) (?expr ...))
     (let ((f ?op))
       ($$l2r (?params ...) (?expr ... f))))))

(define (lowercase-symbol? maybe-sym)
  (and (symbol? maybe-sym)
       (let* ((str (symbol->string maybe-sym))
             (ch (string-ref str 0)))
         (not (char-upper-case? ch)))))

(define hazard-chars (string->list "#`'"))

(define (hazard-symbol? maybe-sym)
  (and (symbol? maybe-sym)
       (let* ((str (symbol->string maybe-sym))
              (ch (string-ref str 0)))
         (member ch hazard-chars))))

(define (safe-symbol-name name)
  (call-with-output-string
    (lambda (out)
      (map (lambda (c)
             (let ((mapping (memv c symbol-character-translations)))
               (display (if mapping
                            (cdr mapping)
                            (string c))
                        out)))
           (string->list name)))))

(define (safe-symbol sym)
  (if (hazard-symbol? sym)
      (string->symbol (safe-symbol-name (symbol->string sym)))
      sym))

(define (kl->scheme expr)
  (match expr
    (`(defun ,name ,args ,body)
     (let ((name (safe-symbol name)))
       `(defun ,name ,(map safe-symbol args)
          ,(quote-expression body args))))
    (else (quote-expression expr '()))))

;; Overrides

(define ($$read-file-as-string filename)
  (call-with-input-file (full-path-for-file filename)
    port->string))

(define ($$read-file-as-bytelist filename)
  (call-with-input-file (full-path-for-file filename)
    (lambda (in)
      (let ((bytes (read-bytevector 1000000 in)))
        (let loop ((position (- (bytevector-length bytes) 1))
                   (result '()))
          (if (< position 0)
              result
              (loop (- position 1)
                    (cons (bytevector-u8-ref bytes position) result))))))))

(define ($$variable? maybe-sym)
  (and (and (symbol? maybe-sym)
       (let* ((str (symbol->string maybe-sym))
             (ch (string-ref str 0)))
         (char-upper-case? ch)))))
