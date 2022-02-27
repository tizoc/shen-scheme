;; Copyright (c) 2012-2021 Bruno Deferrari.  All rights reserved.
;; BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(kl:set '*language* "Scheme")
(kl:set '*implementation* "chez-scheme")
(kl:set '*release* (call-with-values scheme-version-number (lambda (major minor patch) (format "~s.~s.~s" major minor patch))))
(kl:set '*porters* "Bruno Deferrari")
(kl:set '*home-directory* (current-directory))
(kl:set 'shen.*initial-home-directory* (current-directory))

(register-globals)

(kl:global/*sterror* (standard-error-port))
(kl:global/*stinput* (standard-input-port))
(kl:global/*stoutput* (standard-output-port))

(kl:global/shen.*platform-native-call-check*
  (lambda (fname)
    (and (symbol? fname)
         (let ((fname (symbol->string fname)))
           (and (> (string-length fname) 4)
                (char=? #\. (string-ref fname 3))
                (char=? #\s (string-ref fname 0))
                (char=? #\c (string-ref fname 1))
                (char=? #\m (string-ref fname 2)))))))

(kl:_scm.initialize-compiler)
