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

(defun segvar? (Val)
  (scm.and
   (scm.symbol? Val)
   (scm.equal? #\? (scm.string-ref (scm.symbol->string Val) 0))))

(defun shen.pvar? (V)
  (scm.and
   (absvector? V)
   (scm.> (scm.vector-length V) 0)
   (scm.eq? (scm.vector-ref V 0) (scm.quote shen.pvar))))

(defun shen.grammar_symbol? (Val)
  (scm.and
   (scm.symbol? Val)
   (let Strsym (scm.symbol->string Val)
     (scm.and
      (scm.equal? #\< (scm.string-ref Strsym 0))
      (scm.equal? #\> (scm.string-ref Strsym (scm.- (scm.string-length Strsym) 1)))))))

(defun shen.numbyte? (N)
  (scm.and (scm.>= N 48) (scm.<= N 57)))

(defun shen.byte->digit (N)
  (scm.- N 48))

;; Definitions for these are on shen/overwrites-internal.scm

(defun shen.sysfunc? (Val)
  (scm.shen-sysfunc? Val))

(defun shen.walk (Func Val)
  (scm.shen-walk (scm.function-binding Func) Val))

(defun macroexpand (E)
  (scm.macroexpand E))

(defun read-file-as-bytelist (Filename)
  (scm.read-file-as-bytelist Filename))

(defun read-file-as-string (Filename)
  (scm.read-file-as-string Filename))
