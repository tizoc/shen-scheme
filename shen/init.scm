;; Copyright (c) 2012-2015 Bruno Deferrari.  All rights reserved.
;; BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(set (scm.quote *language*) "Scheme")
(set (scm.quote *implementation*) "chibi-scheme")
(set (scm.quote *port*) "0.12")
(set (scm.quote *porters*) "Bruno Deferrari")
(set (scm.quote *stinput*) (scm.current-input-port))
(set (scm.quote *stoutput*) (scm.current-output-port))
(scm.set-shen-environment! (scm.current-environment))
