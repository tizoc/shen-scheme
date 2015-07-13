\* Copyright (c) 2012-2015 Bruno Deferrari.  All rights reserved.
   BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(define open-append
  File -> (let FullPath (scm.path-resolve File (value *home-directory*))
               Mode (scm. "(+ open/create open/write open/append)")
               Fd (scm.open FullPath Mode)
            (scm.open-output-file-descriptor Fd)))
