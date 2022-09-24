\* Copyright (c) 2012-2021 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(define shen-scheme.handle-launcher-result
  [error Message] -> (do (shen.x.launcher.default-handle-result
                            [error Message])
                         ((foreign scm.exit) 1))
  [unknown-arguments | Rest]
  -> (do (shen.x.launcher.default-handle-result
            [unknown-arguments | Rest])
         ((foreign scm.exit) 1))
  Other -> (shen.x.launcher.default-handle-result Other))

(define shen-scheme.run-shen
  Args -> (shen-scheme.handle-launcher-result
           (shen.x.launcher.launch-shen Args)))

(define shen-scheme.find-library
  Name -> ((foreign scm.string-append) ((foreign scm.get-shen-scheme-home-path)) "/libraries/" Name))
