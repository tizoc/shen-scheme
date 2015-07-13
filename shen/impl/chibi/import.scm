;; Copyright (c) 2012-2015 Bruno Deferrari.  All rights reserved.
;; BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(define (import-spec-to-assoc spec)
  (let loop ((rest spec)
             (acc '()))
    (match rest
      ('() (reverse acc))
      (`((,name ,original-name) . ,rest)
       (loop rest (cons (cons name original-name) acc)))
      (`(,(? symbol? name) . ,rest)
       (loop rest (cons (cons name name) acc)))
      (else
       (error "Bad import spec (expects a list of symbols and/or [imported-name original-name]" rest)))))

(define (import-from-module module-path spec)
  (let* ((imports (import-spec-to-assoc spec))
         (module (or (load-module module-path)
                     (error "Module not found" module-path)))
         (menv (module-env module)))
    (%import *shen-environment* menv imports #f)
    spec))
