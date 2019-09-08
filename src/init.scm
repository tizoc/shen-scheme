;; Copyright (c) 2012-2018 Bruno Deferrari.  All rights reserved.
;; BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(kl:set (quote *language*) "Scheme")
(kl:set (quote *implementation*) "chez-scheme")
(kl:set (quote *port*) "0.18")
(kl:set (quote *release*) (call-with-values scheme-version-number (lambda (major minor patch) (format "~s.~s.~s" major minor patch))))
(kl:set (quote *porters*) "Bruno Deferrari")
(kl:set (quote *sterror*) (standard-error-port))
(kl:set (quote *stinput*) (standard-input-port))
(kl:set (quote *stoutput*) (standard-output-port))
(kl:set (quote *home-directory*) (current-directory))
(kl:set (quote shen.*initial-home-directory*) (current-directory))
(kl:_scm.initialize-compiler)
