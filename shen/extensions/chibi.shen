(define open-append
  File -> (let FullPath (scm.path-resolve File (value *home-directory*))
               Mode (scm. "(+ open/create open/write open/append)")
               Fd (scm.open FullPath Mode)
            (scm.open-output-file-descriptor Fd)))
