\* Copyright (c) 2012-2018 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(define shen-scheme.quiet-load
  File -> (let Contents (read-file File)
            (map (/. X (shen.eval-without-macros X)) Contents)))

(define shen-scheme.run-shen
  [Exe "--script" Script | Args]
  -> (do (scm.initialize-shen)
         (set *argv* [Script | Args])
         (shen-scheme.quiet-load Script))
  [Exe "--eval" Code | Args]
  -> (output "~A~%" (eval (head (read-from-string Code))))
  Argv
  -> (do (scm.initialize-shen)
         (set *argv* Argv)
         (shen.shen)))

(define shen-scheme.find-library
  Name -> (scm.string-append (scm.get-shen-scheme-home-path) "/libraries/" Name))
