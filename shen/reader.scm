;; Copyright (c) 2012-2015 Bruno Deferrari.  All rights reserved.
;; BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(define (call-with-input-string str proc)
  (let ((in (open-input-string str)))
    (proc in)))

(define (with-input-from-port port thunk)
  (parameterize ((current-input-port port))
    (thunk)))

(define (with-input-from-string string thunk)
  (call-with-input-string string
    (lambda (in)
      (with-input-from-port in thunk))))

(define (read-error char)
  (error "got unexpected character" char))

(define (parser-error object)
  (error "got unexpected object" object))

(define *start-list* (string-copy "start-list"))
(define *end-list* (string-copy "end-list"))

(define (read-token)
  (let ((char (read-char)))
    (cond
     ((eof-object? char) char)
     ((char-whitespace? char) (read-token))
     ((or (char-numeric? char)
          (eq? char #\-)
          (eq? char #\+)
          (eq? char #\.))
      (read-number char))
     (else
      (case char
        ((#\") (read-string))
        ((#\() *start-list*)
        ((#\)) *end-list*)
        (else
         (read-symbol (list char))))))))

(define (boundary-char? char)
  (or (eof-object? char)
      (char-whitespace? char)
      (char=? #\( char)
      (char=? #\) char)
      (char=? #\" char)))

(define (read-symbol first-chars-reverse)
  (let read-symbol-loop ((res first-chars-reverse))
    (let ((char (peek-char)))
      (if (boundary-char? char)
          (string->symbol (list->string (reverse res)))
          (read-symbol-loop (cons (read-char) res))))))

(define (read-escaped-char)
  (let ((char (read-char)))
    (case char
      ((#\\ #\") char)
      (else
       (read-error char)))))

(define (read-string)
  (let read-string-loop ((res '()))
    (let ((char (read-char)))
      (case char
        ((#\\) (read-string-loop (cons (read-escaped-char) res)))
        ((#\") (list->string (reverse res)))
        (else
         (read-string-loop (cons char res)))))))

(define (numeric? ch)
  (or (char-numeric? ch)
      (eq? ch #\.)
      (eq? ch #\e)
      (eq? ch #\E)))

(define (read-number first-char)
  (let read-number-loop ((res (list first-char)))
    (let ((char (peek-char)))
      (cond ((and (not (eof-object? char)) (numeric? char))
             (read-number-loop (cons (read-char) res)))
            ((boundary-char? char)
             (let ((str (list->string (reverse res))))
               (or (string->number str) (string->symbol str))))
            (else (read-symbol (cons (read-char) res)))))))

(define (read-list)
  (let read-list-loop ((res '()))
    (let ((token (read-token)))
      (if (eq? token *end-list*)
          (reverse res)
          (read-list-loop (cons (read-kl-complete token) res))))))

(define (read-kl-complete token)
  (cond
   ((eq? token *start-list*) (read-list))
   ((or (string? token)
        (number? token)
        (symbol? token))
    token)
   ((eof-object? token) token)
   (else
    (parser-error token))))

(define (read-kl . o)
  (let ((port (and (not (null? o)) (car o))))
    (if port
        (with-input-from-port port
          (lambda () (read-kl-complete (read-token))))
        (read-kl-complete (read-token)))))

(define (read-kl-file filename)
  (call-with-input-file filename
    (lambda (in)
      (let read-loop ((res '()))
        (let ((exp (read-kl in)))
          (if (eof-object? exp)
              (reverse res)
              (read-loop (cons exp res))))))))
