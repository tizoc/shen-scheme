(define shen.quiet-load
  File -> (let Contents (read-file File)
            (map (/. X (shen.eval-without-macros X)) Contents)))

(define sterror
  -> (value *sterror*))

(define exit
  Code -> (scm.exit (scm.exact Code)))

(define command-line
  -> (scm.command-line))

(define open-append
  File -> (let FullPath (scm.path-resolve File (value *home-directory*))
               Mode (scm. "(scm.+ scm.open/create scm.open/write scm.open/append)")
               Fd (scm.open FullPath Mode)
            (scm.open-output-file-descriptor Fd)))

(define stream-position
  Stream -> (scm.file-position Stream))

(define stream-set-position
  Stream Pos -> (do (scm.set-file-position! Stream Pos (scm. "scm.seek/set"))
                    Pos))

(define stream-set-position-from-current
  Stream Pos -> (do (scm.set-file-position! Stream Pos (scm. "scm.seek/cur"))
                    Pos))

(define stream-set-position-from-end
  Stream Pos -> (do (scm.set-file-position! Stream Pos (scm. "scm.seek/end"))
                    Pos))

(declare command-line [--> [list string]])
(declare exit [number --> unit])
(declare open-append [string --> [stream out]])
(declare sterror [--> [stream out]])
(declare stream-position [[stream A] --> number])
(declare stream-set-position [[stream A] --> [number --> number]])
(declare stream-set-position-from-current [[stream A] --> [number --> number]])
(declare stream-set-position-from-end [[stream A] --> [number --> number]])
