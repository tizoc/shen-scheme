(define open-append
  File -> (let FullPath (scm.path-resolve File (value *home-directory*))
            (scm. "(scm.open-output-file FullPath :if-exists :append :if-does-not-exist :create)")))
