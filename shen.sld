(define-library (shen)
  (import

   (rename (shen primitives)
     (kl:if               if)
     (kl:and              and)
     (kl:or               or)
     (kl:cond             cond)
     (kl:intern           intern)
     (kl:pos              pos)
     (kl:tlstr            tlstr)
     (kl:cn               cn)
     (kl:str              str)
     (kl:string?          string?)
     (kl:string->n        string->n)
     (kl:n->string        n->string)
     (kl:set              set)
     (kl:value            value)
     (kl:simple-error     simple-error)
     (kl:trap-error       trap-error)
     (kl:error-to-string  error-to-string)
     (kl:cons             cons)
     (kl:hd               hd)
     (kl:tl               tl)
     (kl:cons?            cons?)
     (kl:defun            defun)
     (kl:lambda           lambda)
     (kl:let              let)
     (kl:=                =)
     (kl:eval-kl          eval-kl)
     (kl:freeze           freeze)
     (kl:type             type)
     (kl:absvector        absvector)
     (kl:<-address        <-address)
     (kl:address->        address->)
     (kl:absvector?       absvector?)
     (kl:read-byte        read-byte)
     (kl:write-byte       write-byte)
     (kl:open             open)
     (kl:close            close)
     (kl:get-time         get-time)
     (kl:+                +)
     (kl:-                -)
     (kl:*                *)
     (kl:/                /)
     (kl:>                >)
     (kl:<                <)
     (kl:>=               >=)
     (kl:<=               <=)
     (kl:number?          number?))

   (prefix (scheme base) $$)
   (prefix (scheme file) $$)
   (prefix (scheme read) $$)
   (prefix (scheme write) $$)
   (prefix (scheme eval) $$)
   (prefix (only (chibi) current-environment import) $$))

  (export shen.shen)

  (include "init.scm")

  (begin
    ;; Avoid warning about shen.demod not being defined yet
    (defun shen.demod (Val) Val))

  (include "compiled/toplevel.kl.scm")
  (include "compiled/core.kl.scm")

  (begin
    (defun shen.sysfunc? (Val)
      ($$shen-sysfunc? Val)))

  (include "compiled/sys.kl.scm")

  (begin
    (defun hash (Val Bound)
      ($$hash Val Bound))

    (defun not (Val)
      ($$not Val))

    (defun boolean? (Val)
      ($$or ($$eq? Val #f)
            ($$eq? Val #t)))

    (defun integer? (Val)
      ($$integer? Val))

    (defun variable? (Val)
      ($$shen-variable? Val))

    (defun symbol? (Val)
      ($$symbol? Val))

    (defun shen.walk (Func Val)
      ($$shen-walk ($$function-binding Func) Val)))

  (include "compiled/sequent.kl.scm")
  (include "compiled/yacc.kl.scm")

  (begin
    (defun shen.grammar_symbol? (Val)
      ($$grammar_symbol? Val)))

  (include "compiled/reader.kl.scm")
  (include "compiled/prolog.kl.scm")

  (begin
    (defun shen.pvar? (V)
       ($$and (absvector? V)
              (> ($$vector-length V) 0)
              ($$eq? (<-address V 0) ($$quote shen.pvar)))))

  (include "compiled/track.kl.scm")
  (include "compiled/load.kl.scm")
  (include "compiled/writer.kl.scm")
  (include "compiled/macros.kl.scm")

  (begin
    (defun macroexpand (E)
      ($$macroexpand E)))

  (include "compiled/declarations.kl.scm")

  (begin
    ;; Overrides
    (defun read-file-as-bytelist (Filename)
      ($$read-file-as-bytelist Filename))

    (defun read-file-as-string (Filename)
      ($$read-file-as-string Filename))

    (defun shen.numbyte? (N)
      (and (>= N 48) (<= N 57)))

    (defun shen.byte->digit (N)
      (- N 48))

    ($$init-*system*))

  (include "compiled/types.kl.scm")
  (include "compiled/t-star.kl.scm")

  (begin
    (cd ".")))
