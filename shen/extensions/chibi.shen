(define open-append
  File -> (let FullPath (scm.path-resolve File (value *home-directory*))
               Mode (scm. "(scm.+ scm.open/create scm.open/write scm.open/append)")
               Fd (scm.open FullPath Mode)
            (scm.open-output-file-descriptor Fd)))
