(defun hash (Val Bound)
  (let Res ($$hash Val Bound)
    ($$if ($$eq? 0 Res) 1 Res)))

(defun not (Val)
  ($$not Val))

(defun boolean? (Val)
  ($$boolean? Val))

(defun integer? (Val)
  ($$integer? Val))

(defun variable? (Val)
  ($$and ($$symbol? Val)
         ($$char-upper-case? ($$string-ref ($$symbol->string Val) 0))))

(defun symbol? (Val)
  ($$symbol? Val))

(defun segvar? (Val)
  ($$and ($$symbol? Val)
         ($$equal? #\? ($$string-ref ($$symbol->string Val) 0))))

(defun shen.pvar? (V)
  ($$and (absvector? V)
         ($$> ($$vector-length V) 0)
         ($$eq? ($$vector-ref V 0) ($$quote shen.pvar))))

(defun shen.grammar_symbol? (Val)
  ($$and ($$symbol? Val)
         (let Strsym ($$symbol->string Val)
           ($$and ($$equal? #\< ($$string-ref Strsym 0))
                  ($$equal? #\> ($$string-ref Strsym ($$- ($$string-length Strsym) 1)))))))

(defun shen.numbyte? (N)
  ($$and ($$>= N 48) ($$<= N 57)))

(defun shen.byte->digit (N)
  ($$- N 48))

;; Definitions for these are on shen/overwrites-internal.scm

(defun shen.sysfunc? (Val)
  ($$shen-sysfunc? Val))

(defun shen.walk (Func Val)
  ($$shen-walk ($$function-binding Func) Val))

(defun macroexpand (E)
  ($$macroexpand E))

(defun read-file-as-bytelist (Filename)
  ($$read-file-as-bytelist Filename))

(defun read-file-as-string (Filename)
  ($$read-file-as-string Filename))
