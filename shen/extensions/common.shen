\* Copyright (c) 2012-2015 Bruno Deferrari.  All rights reserved.
   BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(define shen.quiet-load
  File -> (let Contents (read-file File)
            (map (/. X (shen.eval-without-macros X)) Contents)))

(define command-line
  -> (scm.command-line))

(define stream-position
  Stream -> (scm.file-position Stream))

(define stream-set-position
  Stream Pos -> (do (scm.set-file-position! Stream Pos (scm. "seek/set"))
                    Pos))

(define stream-set-position-from-current
  Stream Pos -> (do (scm.set-file-position! Stream Pos (scm. "seek/cur"))
                    Pos))

(define stream-set-position-from-end
  Stream Pos -> (do (scm.set-file-position! Stream Pos (scm. "seek/end"))
                    Pos))

(declare command-line [--> [list string]])
(declare open-append [string --> [stream out]])
(declare stream-position [[stream A] --> number])
(declare stream-set-position [[stream A] --> [number --> number]])
(declare stream-set-position-from-current [[stream A] --> [number --> number]])
(declare stream-set-position-from-end [[stream A] --> [number --> number]])
