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
     (kl:pr               pr)
     (kl:read-byte        read-byte)
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

   (prefix (shen reader) $$)

   (prefix (scheme base) $$)
   (prefix (scheme file) $$)
   (prefix (scheme read) $$)
   (prefix (scheme write) $$)
   (prefix (scheme eval) $$)
   (prefix (only (scheme) current-environment) $$))

  (export shen-shen)

  (include "init.scm")

  (begin      
    ($$define ($$eval-kl-file filename)
      ($$for-each $$display ($$list "Loading " filename " ...\n"))
      ($$for-each eval-kl ($$read-kl-file filename)))

    ($$eval-kl-file "toplevel.kl")
    ($$eval-kl-file "core.kl")

    (defun shen-sysfunc? (Val)
      ($$shen-sysfunc? Val))

    ($$eval-kl-file "sys.kl")

    (defun hash (Val Bound)
      ($$hash Val Bound))

    (defun not (Val)
      ($$not Val))

    (defun boolean? (Val)
      ($$or ($$eq? Val #f)
            ($$eq? Val #t)
            ($$eq? Val ($$quote true))
            ($$eq? Val ($$quote false))))

    (defun integer? (Val)
      ($$integer? Val))

    (defun variable? (Val)
      ($$shen-variable? Val))

    (defun symbol? (Val)
      (and ($$symbol? Val)
           (not (or ($$eq? Val ($$quote true))
                    ($$eq? Val ($$quote false))))))

    (defun shen-walk (Func Val)
      ($$shen-walk ($$function-binding Func) Val))

    ($$eval-kl-file "sequent.kl")
    ($$eval-kl-file "yacc.kl")

    (defun shen-segvar? (Val)
      ($$segvar? Val))

    (defun shen-grammar_symbol? (Val)
      ($$grammar_symbol? Val))

    ($$eval-kl-file "reader.kl")
    ($$eval-kl-file "prolog.kl")
    ($$eval-kl-file "track.kl")
    ($$eval-kl-file "load.kl")
    ($$eval-kl-file "writer.kl")
    ($$eval-kl-file "macros.kl")

    (defun macroexpand (E)
      ($$macroexpand E))

    ($$eval-kl-file "declarations.kl")

    ;; Overrides
    (defun read-file-as-bytelist (Filename)
      ($$read-file-as-bytelist Filename))

    (defun read-file-as-string (Filename)
      ($$read-file-as-string Filename))

    (defun shen-digit-byte? (N)
      (and (>= N 48) (<= N 57)))

    (defun shen-byte->digit (N)
      (- N 48))

    ($$init-*system*)

    ($$eval-kl-file "types.kl")
    ($$eval-kl-file "t-star.kl")

    (cd ".")))
