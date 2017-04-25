;; Copyright (c) 2012-2015 Bruno Deferrari.  All rights reserved.
;; BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(kl:set (quote *language*) "Scheme")
(kl:set (quote *implementation*)
        (cond-expand
         (chibi "chibi-scheme")
         (gauche "gauche")))
(kl:set (quote *port*) "0.15")
(kl:set (quote *porters*) "Bruno Deferrari")
(kl:set (quote *sterror*) (current-error-port))
(kl:set (quote *stinput*) (current-input-port))
(kl:set (quote *stoutput*) (current-output-port))
(set-shen-environment! (cond-expand
                        (chibi (current-environment))
                        (gauche (current-module))))
