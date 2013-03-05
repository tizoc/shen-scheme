;; Utils
;;

(define *shen-environment* #f)

(define ($$set-shen-environment! env)
  (set! *shen-environment* env))

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

;; Boolean Operators
;;

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
  (string->symbol name))

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

(define-syntax kl:trap-error
  (syntax-rules ()
    ((_ ?expression ?handler)
     (let ((handler ?handler))
       (call-with-current-continuation
        (lambda (exit)
          (with-exception-handler
           (lambda (exn) (exit (handler exn)))
           (lambda () ?expression))))))))

(define (kl:error-to-string e)
  (call-with-output-string
   (lambda (out)
     (display (error-object-message e) out)
     (display ": " out)
     (write-simple (error-object-irritants e) out))))

;; Lists
;;

(define kl:cons cons)

(define kl:hd car)

(define kl:tl cdr)

(define kl:cons? pair?)

;; Generic Functions
;;

;; symbol->function registry
(define *shen-functions* (make-hash-table eq?))
(define *shen-function-arities* (make-hash-table eq?))

(define (register-function name function)
  (hash-table-set! *shen-functions* name function))

(define (register-function-arity name arity)
  (hash-table-set! *shen-function-arities* name arity))

(define ($$function-arity name)
  (if (symbol? name)
      (hash-table-ref/default *shen-function-arities* name -1)
      -1))

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
        ((and (number? a) (number? b))
         (= a b))
        ;; if eq? was false none of these can result in #t
        ((or (null? a) (null? b) (symbol? a) (symbol? b)) #f)
        ((and (pair? a) (pair? b))
         (and (kl:= (car a) (car b))
              (kl:= (cdr a) (cdr b))))
        ((and (vector? a) (vector? b))
         (vector=? a b))
        (else (equal? a b))))

(define ($$eval-in-shen expr)
  (eval expr *shen-environment*))

(define (kl:eval-kl expr)
  ($$eval-in-shen (kl->scheme expr)))

(define (or-function a b)
  (kl:or a b))

(define (and-function a b)
  (kl:and a b))

(define ($$function-binding maybe-symbol)
  (if (symbol? maybe-symbol)
      (hash-table-ref *shen-functions* maybe-symbol
                      (lambda () (error "undefined function: "
                                        maybe-symbol)))
      maybe-symbol))

(define-syntax kl:freeze
  (syntax-rules ()
    ((_ ?expr) (lambda () ?expr))))

(define (kl:type val type)
  val) ;; FIXME: do something with type

;; Vectors
;;

(define (kl:absvector size)
  (make-vector size 'shen-fail!))

(define kl:<-address vector-ref)

(define (kl:address-> vec loc val)
  (vector-set! vec loc val)
  vec)

(define kl:absvector? vector?)

;; Streams and I/O
;;

(define (kl:pr string out)
  (display string out)
  (flush-output-port out)
  string)

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

(define kl:/ inexact-/)
(define (kl:+ a b) (+ a b))
(define (kl:- a b) (- a b))
(define (kl:* a b) (* a b))
(define (kl:> a b) (> a b))
(define (kl:< a b) (< a b))
(define (kl:>= a b) (>= a b))
(define (kl:<= a b) (<= a b))

(define kl:number? number?)

;; register functions for binding resolution

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
       (pr ,kl:pr)
       (read-byte ,kl:read-byte)
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

(define (initialize-arity-table entries)
  (if (null? entries)
      'done
      (let ((name (car entries))
            (arity (cadr entries)))
        (register-function-arity name arity)
        (initialize-arity-table (cddr entries)))))

(initialize-arity-table
 '(adjoin 2 and 2 append 2 arity 1 assoc 2 boolean? 1 cd 1 compile 3 concat 2 cons 2
   cons? 1 cn 2 declare 2 destroy 1 difference 2 do 2 element? 2 empty? 1
   enable-type-theory 1 interror 2 eval 1 eval-kl 1 explode 1
   external 1 fail-if 2 fail 0 fix 2 findall 5 freeze 1 fst 1 gensym 1 get 3
   address-> 3 <-address 2 <-vector 2 > 2
   >= 2 = 2 hd 1 hdv 1 hdstr 1 head 1 if 3 integer? 1 identical 4 inferences 1
   intoutput 2 make-string 2 intersection 2 length 1 lineread 0 load 1 < 2 <= 2
   vector 1 macroexpand 1 map 2 mapcan 2 intmake-string 2
   maxinferences 1 not 1 nth 2 n->string 1 number? 1 output 2 occurs-check 1
   occurrences 2 occurs-check 1 or 2 package 3 pos 2 print 1 profile 1
   profile-results 1 ps 1 preclude 1 preclude-all-but 1 protect 1 address-> 3
   put 4 reassemble 2 read-file-as-string 1 read-file 1 read-byte 1 remove 2
   reverse 1 set 2 simple-error 1 snd 1 specialise 1
   spy 1 step 1 stinput 1 stoutput 1 string->n 1 string? 1 strong-warning 1
   subst 3 symbol? 1 tail 1 tl 1 tc 1 tc? 1 thaw 1
   track 1 trap-error 2 tuple? 1 type 1 return 3 unprofile 1 unify 4 unify! 4
   union 2 untrack 1 unspecialise 1 vector 1
   vector-> 3 value 1 variable? 1 version 1 warn 1 write-to-file 2 y-or-n? 1
   + 2 * 2 / 2 - 2 == 2 <1> 1 <e> 1
   @p 2 @v 2 @s 2 preclude 1 include 1 preclude-all-but 1 include-all-but 1 where 2))

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

(define *gensym-counter* 0)

(define (gensym prefix)
  (set! *gensym-counter* (+ 1 *gensym-counter*))
  (string->symbol (string-append prefix (number->string *gensym-counter*))))

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
    ((? unbound-in-current-scope? sym) `($$quote ,sym))
    (('let var value body)
     `(let ,var ,(quote-expression value scope)
        ,(quote-expression body (cons var scope))))
    (('cond clauses ...)
     `(cond ,@(quote-cond-clauses clauses scope)))
    (('lambda var body)
     `(lambda ,var ,(quote-expression body (cons var scope))))
    (('do expr1 expr2)
     `($$begin ,(quote-expression expr1 scope) ,(quote-expression expr2 scope)))
    (`(defun ,name ,args ,body)
     `($$eval-in-shen
       ($$quote
        (defun ,name ,args
          ,(quote-expression body args)))))
    ;; inlines fail compares
    (('= expr '(fail)) `($$eq? ,(quote-expression expr scope) ($$quote shen-fail!)))
    (('fail) '($$quote shen-fail!))
    ((op param ...)
     (let* ((arity ($$function-arity op))
            (partial-call? (not (or (= arity -1) (= arity (length param)))))
            (args (map (lambda (exp) (quote-expression exp scope))
                       param))
            (args-list (left-to-right `($$list ,@args))))
       (cond (partial-call?
              `($$call-nested ,($$nest-lambda op arity) ,args-list))
             ((or (pair? op) (not (unbound-in-current-scope? op)))
              (left-to-right
               `($$call-nested ($$function ,(quote-expression op scope)) ,args-list)))
             (else
              (left-to-right (cons op args))))))
    (else expr)))

(define ($$nest-lambda callable arity)
  (define (merge-args f arg)
    (if (pair? f)
        (append f (list arg))
        (list f arg)))

  (if (<= arity 0)
      callable
      (let ((aname (gensym "Y")))
        `(lambda ,aname
           ,($$nest-lambda (merge-args callable aname) (- arity 1))))))

(define ($$call-nested f args)
  (if (null? args)
      f
      ($$call-nested (f (car args)) (cdr args))))

(define (arity-error? e)
  (string-prefix? "not enough args" (error-object-message e)))

(define (handle-arity-error exn f args)
  (if (and (arity-error? exn) (> ($$function-arity f) (length args)))
      ($$call-nested
       (kl:eval-kl ($$nest-lambda f ($$function-arity f))) args)
      (raise exn)))

(define ($$function f)
  (if (not (symbol? f))
      f
      (lambda args
        (call-with-current-continuation
         (lambda (exit)
           (with-exception-handler
            (lambda (exn) (exit (handle-arity-error exn f args)))
            (lambda () (apply ($$function-binding f) args))))))))

;; Enforce left-to-right evaluation if needed
(define (left-to-right expr)
  (if (or (memq (car expr) '(trap-error set and or if freeze thaw))
          (< (length (filter pair? expr)) 2))
      expr
      `($$l2r ,expr ())))

(define-syntax $$l2r
  (syntax-rules ()
    ((_ () ?expr) ?expr)
    ((_ (?op ?params ...) (?expr ...))
     (let ((f ?op))
       ($$l2r (?params ...) (?expr ... f))))))

(define (kl->scheme expr)
  (match expr
    (`(defun ,name ,args ,body)
     `(defun ,name ,args
        ,(quote-expression body args)))
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

(define ($$shen-variable? maybe-sym)
  (and (symbol? maybe-sym)
       (char-upper-case? (string-ref (symbol->string maybe-sym) 0))))

(define ($$segvar? maybe-sym)
  (and (symbol? maybe-sym)
       (equal? #\? (string-ref (symbol->string maybe-sym) 0))))

(define ($$grammar_symbol? maybe-sym)
  (and (symbol? maybe-sym)
       (let ((strsym (symbol->string maybe-sym)))
         (and (equal? #\< (string-ref strsym 0))
              (equal? #\> (string-ref strsym (- (string-length strsym) 1)))))))

(define shen-*system* (make-hash-table eq?))

(define ($$init-*system*)
  (for-each
   (lambda (sym) (hash-table-set! shen-*system*
                                  (case sym
                                    ((#t) 'true)
                                    ((#f) 'false)
                                    (else sym)) #t))
   (kl:value 'shen-*system*)))

(define ($$shen-sysfunc? val)
  (hash-table-ref/default shen-*system* val #f))

(define ($$hash val bound)
  (let ((res (hash val bound)))
    (if (eq? 0 res) 1 res)))

(define ($$shen-walk func val)
  (if (pair? val)
      (func (map (lambda (subexp) ($$shen-walk func subexp)) val))
      (func val)))

(define (compose funcs value)
  (if (null? funcs)
      value
      (compose (cdr funcs) ((car funcs) value))))

(define ($$macroexpand expr)
  (define macros (map $$function-binding (kl:value '*macros*)))

  (define (expand expr)
    (let ((transformed (compose macros expr)))
      (if (eq? expr transformed)
          expr
          ($$shen-walk expand transformed))))

  (expand expr))
