;; Copyright (c) 2012-2015 Bruno Deferrari.  All rights reserved.
;; BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(define (import-spec-to-assoc spec)
  (let loop ((rest spec)
             (acc '()))
    (match rest
      ('() (reverse acc))
      (`((,name ,original-name) . ,rest)
       (loop rest (cons (list name original-name) acc)))
      (`(,(? symbol? name) . ,rest)
       (loop rest (cons (list name name) acc)))
      (else
       (error "Bad import spec (expects a list of symbols and/or [imported-name original-name]" rest)))))

(define (import-from-module module-name spec)
  (let* ((imports (import-spec-to-assoc spec))
         (_ (eval-in-shen `(require ,(module-name->path module-name))))
         (module (find-module module-name)))
    (for-each (lambda (imp)
                (let ((newname (car imp))
                      (oldname (cadr imp)))
                  (eval-in-shen
                   `(define ,newname (with-module ,module ,oldname)))))
              imports)
    spec))
