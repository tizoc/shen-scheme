(import (chezscheme))

(define (true? value)
  (if (member (string-downcase value) '("1" "true" "yes" "on")) #t #f))

(define (usage)
  (display
    "usage: build-runtime.ss PROGRAM-SOURCE PROGRAM-OBJECT RUNTIME-SOURCE RUNTIME-OBJECT OUTPUT-OBJECT OPTIMIZE DEBUG INSPECTOR SOURCE-INFO\n"
    (current-error-port))
  (exit 2))

(define args (cdr (command-line)))

(unless (= (length args) 9)
  (usage))

(define (build-runtime program-source program-object runtime-source
                       runtime-object output-object optimize debug
                       inspector source-info)
  (let ([root (path-parent (path-parent runtime-object))])
    (parameterize ([optimize-level (string->number optimize)]
                   [debug-level (string->number debug)]
                   [generate-inspector-information (true? inspector)]
                   [generate-procedure-source-information (true? source-info)]
                   [compile-file-message #t])
      (compile-file runtime-source runtime-object)
      (parameterize ([library-directories
                      (cons (cons root root) (library-directories))])
        (compile-file program-source program-object))
      (when (file-exists? output-object)
        (delete-file output-object))
      (concatenate-object-files output-object runtime-object program-object))))

(apply build-runtime args)
