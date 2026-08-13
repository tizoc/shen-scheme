;; Copyright (c) 2012-2021 Bruno Deferrari.  All rights reserved.
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

(define (get-failure-error var)
  (raise-error 'value "variable has no value" var))

(define (get-failure-default _var)
  *hash-table-default*)

(define (kl:value var)
  (shen-global-get var get-failure-error))

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
         (result (if (pair? scm-expr)
                     (eval scm-expr)
                     scm-expr)))
    (if (and (pair? scm-expr) (eq? (car scm-expr) 'define))
        (function-name scm-expr)
        result)))

(define (shen-scheme-write-forms scheme-file forms include-chez-import?)
  (when (file-exists? scheme-file)
    (delete-file scheme-file))
  (call-with-output-file scheme-file
    (lambda (out)
      (when include-chez-import?
        (display "(import (chezscheme))" out)
        (newline out)
        (newline out))
      (let loop ((forms forms))
        (unless (null? forms)
          (pretty-print (car forms) out)
          (newline out)
          (loop (cdr forms)))))))

(define (shen-scheme-write-native-forms scheme-file forms)
  (shen-scheme-write-forms scheme-file forms #t))

(define (shen-scheme-compile-file-message?)
  (not (shen-global-get '*hush* (lambda (_) #f))))

(define (shen-scheme-compile-scheme-file scheme-file object optimize debug inspector source-info wpo)
  (parameterize ([optimize-level optimize]
                 [debug-level debug]
                 [generate-inspector-information inspector]
                 [generate-procedure-source-information source-info]
                 [generate-wpo-files wpo]
                 [compile-file-message (shen-scheme-compile-file-message?)])
    (compile-file scheme-file object))
  object)

(define (shen-scheme-compile-native-forms scheme-file object forms optimize debug inspector source-info wpo)
  (shen-scheme-write-native-forms scheme-file forms)
  (shen-scheme-compile-scheme-file scheme-file object optimize debug inspector source-info wpo))

(define (shen-scheme-compile-native-forms-direct object forms optimize debug inspector source-info wpo)
  (parameterize ([optimize-level optimize]
                 [debug-level debug]
                 [generate-inspector-information inspector]
                 [generate-procedure-source-information source-info]
                 [generate-wpo-files wpo]
                 [compile-file-message (shen-scheme-compile-file-message?)])
    (compile-to-file (cons '(import (chezscheme)) forms) object))
  object)

(define (shen-scheme-parent-directory directory)
  (let ((len (string-length directory)))
    (let loop ((i (- len 1)))
      (cond ((<= i 0) #f)
            ((char=? (string-ref directory i) #\/)
             (substring directory 0 i))
            (else (loop (- i 1)))))))

(define (shen-scheme-ensure-directory-path directory)
  (unless (file-exists? directory)
    (let ((parent (shen-scheme-parent-directory directory)))
      (when parent
        (shen-scheme-ensure-directory-path parent)))
    (mkdir directory)))

(define (shen-scheme-replace-suffix filename suffix)
  (let* ((len (string-length filename))
         (dot-index
          (let loop ((i (- len 1)))
            (cond ((< i 0) #f)
                  ((char=? (string-ref filename i) #\.) i)
                  (else (loop (- i 1)))))))
    (if dot-index
        (string-append (substring filename 0 dot-index) suffix)
        (string-append filename suffix))))

(define (shen-scheme-native-app-module-program-forms module-forms)
  (if (null? module-forms)
      '()
      (let ((module-form (car module-forms)))
        (cons module-form
              (cons `(import ,(cadr module-form))
                    (shen-scheme-native-app-module-program-forms
                     (cdr module-forms)))))))

(define (shen-scheme-compile-native-app root-dir module-forms program-forms object optimize debug inspector source-info wpo)
  (shen-scheme-ensure-directory-path root-dir)
  (let* ((program-file (string-append root-dir "/main.ss"))
         (program-object (if wpo
                             (string-append root-dir "/main.so")
                             object))
         (program-wpo (shen-scheme-replace-suffix program-object ".wpo")))
    (shen-scheme-write-forms program-file
                             (append
                              '((import (chezscheme) (shen-scheme runtime)))
                              (shen-scheme-native-app-module-program-forms
                               module-forms)
                              program-forms)
                             #f)
    (parameterize ([optimize-level optimize]
                   [debug-level debug]
                   [generate-inspector-information inspector]
                   [generate-procedure-source-information source-info]
                   [generate-wpo-files wpo]
                   [compile-file-message (shen-scheme-compile-file-message?)]
                   [library-directories
                    (cons (cons (get-shen-scheme-home-path)
                                (get-shen-scheme-home-path))
                          (library-directories))])
      (compile-program program-file program-object)
      (if wpo
          (list object (compile-whole-program program-wpo object))
          (list object '())))))

(define shen-scheme-native-load-init (make-parameter #t))

(define (shen-scheme-native-load-init?)
  (shen-scheme-native-load-init))

(define (shen-scheme-load-compiled object)
  (load object)
  object)

(define (shen-scheme-load-compiled-for-compilation object)
  (parameterize ([shen-scheme-native-load-init #f])
    (load object))
  object)

(define shen-scheme-fnv64-offset 14695981039346656037)
(define shen-scheme-fnv64-prime 1099511628211)
(define shen-scheme-fnv64-modulus (expt 2 64))

(define (shen-scheme-fnv64-update hash byte)
  (modulo (* (bitwise-xor hash byte) shen-scheme-fnv64-prime)
          shen-scheme-fnv64-modulus))

(define (shen-scheme-hash-bytes bytes seed)
  (let loop ((bytes bytes)
             (hash seed))
    (if (null? bytes)
        hash
        (loop (cdr bytes)
              (shen-scheme-fnv64-update hash (car bytes))))))

(define (shen-scheme-hash-string string seed)
  (let ((len (string-length string)))
    (let loop ((i 0)
               (hash seed))
      (if (= i len)
          hash
          (loop (+ i 1)
                (shen-scheme-fnv64-update hash (char->integer (string-ref string i))))))))

(define (shen-scheme-hash->hex hash)
  (let ((hex (number->string hash 16)))
    (string-append (make-string (- 16 (string-length hex)) #\0) hex)))

(define (shen-scheme-file-hash filename)
  (shen-scheme-hash->hex
   (shen-scheme-hash-bytes
    (read-file-as-bytelist filename)
    shen-scheme-fnv64-offset)))

(define (shen-scheme-native-source-key source)
  (list (full-path-for-file source) (shen-scheme-file-hash source)))

(define (shen-scheme-resolve-module-source declaration source)
  (if (path-absolute? source)
      source
      (path-build
       (path-parent (full-path-for-file (with-home-directory declaration)))
       source)))

(define (shen-scheme-relative-path? path)
  (and (> (string-length path) 0)
       (not (path-absolute? path))))

(define (shen-scheme-native-key sources options)
  (shen-scheme-hash->hex
   (shen-scheme-hash-string
    (call-with-string-output-port
     (lambda (out)
       (write (list (map shen-scheme-native-source-key sources) options) out)))
    shen-scheme-fnv64-offset)))

(define (shen-scheme-delete-file-if-exists filename)
  (when (file-exists? filename)
    (delete-file filename))
  #t)

;; Streams and I/O
;;

(define (with-home-directory filename)
  (string-append (kl:value '*home-directory*) filename))

(define (kl:open filename direction)
  (let ((full-path (full-path-for-file (with-home-directory filename))))
    (case direction
      ((in) (if (file-exists? full-path)
                (open-binary-input-file full-path)
                (raise-error 'open "File does not exist" full-path)))
      ((out) (open-binary-output-file full-path))
      (else (raise-error 'open "Invalid direction" direction)))))

(define (kl:close stream)
  (cond
   ((input-port? stream) (close-input-port stream) '())
   ((output-port? stream) (close-output-port stream) '())
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
  (let ((result (shen-global-get var get-failure-default)))
    (if (eq? result *hash-table-default*)
        (default)
        result)))

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
