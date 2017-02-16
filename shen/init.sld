;; Copyright (c) 2012-2015 Bruno Deferrari.  All rights reserved.
;; BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(define-library (shen init)
  (import

   (shen primitives)
   (shen overwrites-internal)

   (scheme base)
   (scheme write)
   (scheme char)
   (scheme file)
   (scheme eval)
   (scheme process-context))

  (cond-expand
   (chibi
    (import (only (chibi pathname) path-resolve)
            (only (chibi filesystem)
              change-directory current-directory
              open open/append open/write open/create
              open-output-file-descriptor)
            (only (chibi) current-environment)
            (only (chibi io)
              seek/end seek/cur seek/set
              set-file-position! file-position)))
   (gauche
    (import (gauche base)
            (only (file util) expand-path build-path absolute-path?)
            (only (rename (gauche base)
                    (SEEK_CUR seek/cur)
                    (SEEK_SET seek/set)
                    (SEEK_END seek/end)
                    (port-tell file-position)
                    (port-seek set-file-position!)
                    (sys-getcwd current-directory)
                    (sys-chdir change-directory))
              current-directory
              change-directory
              seek/cur seek/set seek/end
              file-position set-file-position!
              current-directory change-directory))
    (begin
      (define (path-resolve subpath base)
        (let ((subpath (expand-path subpath)))
          (if (absolute-path? subpath)
              subpath
              (build-path (expand-path base) subpath)))))))

  ;; Import after other gauche stuff
  (import (only (srfi 69) hash))

  (export kl:shen.shen
          kl:eval
          kl:eval-kl
          kl:shen.quiet-load)

  (include "impl/init.scm")

  ;; Avoid warning about shen.demod not being defined yet
  (begin (define (kl:shen.demod Val) Val))

  (include "compiled/toplevel.kl.scm")
  (include "compiled/core.kl.scm")
  (include "compiled/sys.kl.scm")
  (include "compiled/sequent.kl.scm")
  (include "compiled/yacc.kl.scm")
  (include "compiled/reader.kl.scm")
  (include "compiled/prolog.kl.scm")
  (include "compiled/track.kl.scm")
  (include "compiled/load.kl.scm")
  (include "compiled/writer.kl.scm")
  (include "compiled/macros.kl.scm")
  (include "compiled/declarations.kl.scm")
  (include "compiled/types.kl.scm")
  (include "compiled/t-star.kl.scm")
  (include "compiled/extensions-common.kl.scm")

  (cond-expand
   (chibi (include "compiled/extensions-chibi.kl.scm"))
   (gauche (include "compiled/extensions-gauche.kl.scm")))

  (begin
    (kl:cd "")))
