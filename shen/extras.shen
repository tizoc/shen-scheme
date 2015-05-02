(define shen.quiet-load
  File -> (let Contents (read-file File)
            (map (/. X (shen.eval-without-macros X)) Contents)))

(define sterror
  -> (value *sterror*))

(define exit
  Code -> (scm.exit Code))

(define command-line
  -> (scm.command-line))

(define open-append
  File -> (let FullPath (scm.path-resolve File (value *home-directory*))
               Mode (scm. "(scm.+ scm.open/create scm.open/write scm.open/append)")
               Fd (scm.open FullPath Mode)
            (scm.open-output-file-descriptor Fd)))

(declare open-append [string --> [stream out]])
