(import (chezscheme) (shen-scheme runtime))

(define (check label expected actual)
  (unless (equal? expected actual)
    (error 'string-boundaries "~a: expected ~s, got ~s" label expected actual))
  (printf "[OK]    ~a\n" label))

;; Build expectations independently of the runtime's conversion helpers.
(define (bytes unicode)
  (list->string (map integer->char (bytevector->u8-list (string->utf8 unicode)))))

(define (write-source filename text)
  (call-with-port (open-file-output-port filename (file-options no-fail))
    (lambda (out) (put-bytevector out (string->utf8 text)))))

(define (caught thunk)
  (guard (e [else e])
    (thunk)
    (error 'string-boundaries "expected an exception")))

(define (ensure-directory directory)
  (unless (file-exists? directory) (mkdir directory)))

(define root "_build/string-boundaries")
(ensure-directory root)
(kl:set '*hush* #f)

(check "Unicode enters Shen as UTF-8 bytes"
  (bytes "éλ雪") (unicode->shen-string "éλ雪"))
(check "UTF-8 bytes leave Shen as Unicode"
  "éλ雪" (shen-string->unicode (bytes "éλ雪")))
(let ((text (string-append (string (integer->char #xfeff)) "éλ")))
  (check "UTF-8 conversion preserves a leading U+FEFF as data"
    text (shen-string->unicode (bytes text))))
(check "malformed UTF-8 filenames raise instead of guessing an encoding"
  #t
  (i/o-decoding-error? (caught (lambda () (kl:open (string (integer->char 255)) 'in)))))

;; Exercise both kinds of Latin-1 characters and wider Unicode in directory
;; names. In particular, decoding the Unicode name "Ã©" again selects "é".
(for-each
  (lambda (name)
    (let* ((directory (string-append root "/" name))
           (filename (string-append directory "/naïve λ.txt"))
           (shen-filename (bytes filename))
           (content (bytes "λ雪 café")))
      (ensure-directory directory)
      (kl:write-to-file shen-filename content)
      (check "a literal filename gets its UTF-8 name on disk"
        #t (file-exists? filename))
      (check "read-file-as-string accepts a UTF-8 filename"
        content (kl:read-file-as-string shen-filename))
      (check "read-file-as-bytelist accepts a UTF-8 filename"
        (bytevector->u8-list (string->utf8 "λ雪 café"))
        (kl:read-file-as-bytelist shen-filename))
      (check "native file-exists? accepts a UTF-8 filename"
        #t (kl:shen-scheme.file-exists? shen-filename))
      (kl:shen-scheme.delete-file-if-exists shen-filename)
      (check "native delete accepts a UTF-8 filename"
        #f (file-exists? filename))))
  '("λ" "é" "Ã©"))

(let* ((message (bytes "éλ雪"))
       (shen-error (caught (lambda () (kl:eval (list 'simple-error message))))))
  (check "Shen errors retain their UTF-8 message bytes"
    message (kl:error-to-string shen-error))
  (check "Shen errors retain their raise continuation"
    #t (continuation-condition? shen-error))
  (check "Chez error messages enter Shen as UTF-8 bytes"
    message (kl:error-to-string (caught (lambda () (error 'test "éλ雪")))))
  (check "native condition printers receive Unicode Shen messages"
    "éλ雪" (condition-message shen-error)))

(let* ((missing (bytes (string-append root "/missing-éλ.shen")))
       (e (caught (lambda () (kl:open missing 'in)))))
  (check "missing filenames remain UTF-8 in caught errors"
    (bytes (format "File does not exist: ~s"
             (string-append (current-directory) (string (directory-separator))
                            root "/missing-éλ.shen")))
    (kl:error-to-string e)))

(let ((source (string-append root "/λ/chargé.shen")))
  (write-source source "(define byte-boundary-loaded -> \"λ雪 café\")\n")
  (kl:load (bytes source))
  (check "load reads a source named by UTF-8 bytes"
    (bytes "λ雪 café") (kl:eval '(byte-boundary-loaded)))
  (kl:shen-scheme.run-shen (list "shen-scheme" "script" (bytes source)))
  (check "the Shen launcher accepts arguments that are already byte strings"
    (bytes source) (car (kl:value '*argv*))))

;; Native builds are unavailable in Petite; all preceding I/O checks run there.
(unless (equal? (getenv "SHEN_SCHEME_RUNTIME") "petite")
  (let* ((directory (string-append root "/λ"))
         (source (string-append directory "/naïve.shen"))
         (declaration (string-append directory "/demo.shenmod"))
         (object (bytes (string-append directory "/résultat.so")))
         (scheme (bytes (string-append directory "/émis.scm"))))
    (write-source source
      "(define byte-boundary-answer -> 42)\n(define byte-boundary-error -> (simple-error \"éλ\"))\n")
    (write-source declaration
      "(shen.module (version 1) (name byte.boundary) (sources tc- \"naïve.shen\"))\n")
    (check "direct compilation returns the original filename bytes"
      object (kl:shen-scheme.compile-file (bytes source) object))
    (kl:shen-scheme.native-load-compiled-for-compilation object)
    (check "compile-time loading accepts a UTF-8 filename"
      42 (kl:eval '(byte-boundary-answer)))
    (kl:shen-scheme.load-compiled object)
    (check "directly compiled objects load through a UTF-8 filename"
      42 (kl:eval '(byte-boundary-answer)))
    (check "native Shen errors retain UTF-8 message bytes"
      (bytes "éλ")
      (kl:error-to-string (caught (lambda () (kl:eval '(byte-boundary-error))))))
    (check "Scheme-emitting compilation accepts UTF-8 filenames"
      object (kl:shen-scheme.compile-file/emit (bytes source) object scheme))
    (check "emitted Scheme gets its UTF-8 name on disk"
      #t (file-exists? (shen-string->unicode scheme)))
    (kl:shen-scheme.compile-module (bytes declaration) object)
    (kl:shen-scheme.load-compiled object)
    (check "module resolution keeps Unicode directories and source bytes consistent"
      42 (kl:eval '(byte-boundary-answer)))
    (kl:shen-scheme.compile-module
      (bytes declaration) (bytes (string-append directory "/byte.boundary.so")))
    (kl:shen-scheme.load-module (bytes declaration) (bytes directory) (bytes directory))
    (check "module object checks and loads accept Unicode directories"
      42 (kl:eval '(byte-boundary-answer)))
    (let ((app (bytes (string-append directory "/module-app.so"))))
      (kl:shen-scheme.build-module-app (bytes declaration) (bytes directory) app)
      (kl:shen-scheme.load-compiled app)
      (check "module app builds accept Unicode declarations and output files"
        42 (kl:eval '(byte-boundary-answer))))
    (for-each
      (lambda (wpo)
        (let ((app (bytes (string-append directory (if wpo "/app-wpo.so" "/app.so")))))
          (kl:shen-scheme.build-app/emit/options
            (bytes source) '() app (bytes (string-append directory "/bâtir"))
            (kl:shen-scheme.native-compile-options) wpo)
          (kl:shen-scheme.load-compiled app)
          (check "app builds accept UTF-8 sources, output files and build directories"
            42 (kl:eval '(byte-boundary-answer)))))
      '(#f #t)))
  (let* ((correct (string-append root "/Ã©"))
         (wrong (string-append root "/é"))
         (declaration (string-append correct "/demo.shenmod"))
         (object (bytes (string-append correct "/résultat.so"))))
    (write-source (string-append correct "/source.shen")
      "(define byte-boundary-selected -> 42)\n")
    (write-source (string-append wrong "/source.shen")
      "(define byte-boundary-selected -> 99)\n")
    (write-source declaration
      "(shen.module (version 1) (name byte.boundary.selected) (sources tc- \"source.shen\"))\n")
    (kl:shen-scheme.compile-module (bytes declaration) object)
    (kl:shen-scheme.load-compiled object)
    (check "module paths are decoded exactly once"
      42 (kl:eval '(byte-boundary-selected)))))
