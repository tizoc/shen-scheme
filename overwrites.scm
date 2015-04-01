;; Copyright (c) 2012-2015 Bruno Deferrari.  All rights reserved.
;; BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause

(defun cd (Val)
  (let Dir (if (= Val "") "" (scm.string-append Val "/"))
    (let NewDir (scm.change-directory Dir)
      (set (scm.quote *home-directory*) (scm.current-directory)))))

(defun hash (Val Bound)
  (let Res (scm.hash Val Bound)
    (scm.if (scm.eq? 0 Res) 1 Res)))

(defun not (Val)
  (scm.not Val))

(defun boolean? (Val)
  (scm.boolean? Val))

(defun integer? (Val)
  (scm.integer? Val))

(defun variable? (Val)
  (scm.and
   (scm.symbol? Val)
   (scm.char-upper-case? (scm.string-ref (scm.symbol->string Val) 0))))

(defun symbol? (Val)
  (scm.symbol? Val))

(defun shen.pvar? (V)
  (scm.and
   (absvector? V)
   (scm.> (scm.vector-length V) 0)
   (scm.eq? (scm.vector-ref V 0) (scm.quote shen.pvar))))

(defun shen.grammar_symbol? (Val)
  (scm.shen-grammar_symbol? Val))

(defun shen.numbyte? (N)
  (scm.and (scm.>= N 48) (scm.<= N 57)))

(defun shen.byte->digit (N)
  (scm.- N 48))

(defun shen.lookup-func (F SymbolTable)
  (let Entry (scm.assq F SymbolTable)
    (scm.if
     Entry
     (scm.cdr Entry)
     (simple-error (scm.string-append
                    (scm.symbol->string F)
                    " has no lambda expansion\n")))))

;; Definitions for these are on shen/overwrites-internal.scm

(defun shen.sysfunc? (Val)
  (scm.shen-sysfunc? Val))

(defun read-file-as-bytelist (Filename)
  (scm.read-file-as-bytelist Filename))

(defun read-file-as-string (Filename)
  (scm.read-file-as-string Filename))
