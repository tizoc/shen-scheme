(import (chezscheme) (shen-scheme runtime))

(define (check label expected actual)
  (unless (equal? expected actual)
    (error 'byte-strings "~a: expected ~s, got ~s" label expected actual))
  (printf "[OK]    ~a\n" label))

(define (printed-bytes string)
  (call-with-bytevector-output-port
    (lambda (out)
      (check "pr returns its input string" string (kl:pr string out)))))

(define (write-bytes filename bytes)
  (call-with-port
    (open-file-output-port filename (file-options no-fail))
    (lambda (out) (put-bytevector out bytes))))

(define byte-values (iota 256))
(define byte-string (list->string (map integer->char byte-values)))
(define byte-file "_build/byte-strings.bin")
(define source-file "_build/byte-strings-source.shen")

(kl:set '*hush* #f)

(check "binary pr preserves every byte"
  (u8-list->bytevector byte-values)
  (printed-bytes byte-string))
(check "binary pr handles an empty string" #vu8() (printed-bytes ""))
;; pr already traps output errors and returns its input. Wide characters
;; follow that path rather than inventing a second encoding for byte output.
(check "binary pr does not UTF-8-encode wide characters"
  #vu8() (printed-bytes (string (integer->char 955))))

(check "textual pr preserves Scheme Unicode strings"
  "éλ"
  (call-with-string-output-port
    (lambda (out) (kl:pr "éλ" out))))

(let ((hush (kl:value '*hush*)))
  (dynamic-wind
    (lambda () (kl:set '*hush* #t))
    (lambda ()
      (check "hushed pr writes no bytes" #vu8() (printed-bytes byte-string)))
    (lambda () (kl:set '*hush* hush))))

;; Include a UTF-8 BOM, invalid UTF-8 bytes, CRLF and a trailing CR. None of
;; these should be decoded, replaced, stripped or normalized by a byte read.
(define input-values (append '(239 187 191) byte-values '(13 10 13)))
(write-bytes byte-file (u8-list->bytevector input-values))
(check "read-file-as-string preserves raw bytes and line endings"
  input-values
  (map char->integer (string->list (kl:read-file-as-string byte-file))))
(check "read-file-as-string agrees with read-file-as-bytelist"
  (kl:read-file-as-bytelist byte-file)
  (map char->integer (string->list (kl:read-file-as-string byte-file))))

(write-bytes byte-file (printed-bytes byte-string))
(check "binary pr and read-file-as-string round-trip every byte"
  byte-string (kl:read-file-as-string byte-file))
(write-bytes byte-file #vu8())
(check "read-file-as-string handles an empty file"
  "" (kl:read-file-as-string byte-file))

;; Exercise the Shen reader as well as pr: the expected bytes are the UTF-8
;; source bytes, while the Shen literal contains one character per byte.
(write-bytes source-file
  (string->utf8 "(pr \"λ雪 café\" (stoutput))\n"))
(check "a UTF-8 source literal reaches binary output unchanged"
  (string->utf8 "λ雪 café")
  (call-with-bytevector-output-port
    (lambda (out)
      (parameterize ([kl:global/*stoutput* out])
        (kl:shen.x.launcher.quiet-load source-file)))))

(delete-file byte-file)
(delete-file source-file)
