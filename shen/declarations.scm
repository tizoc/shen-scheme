;; symbol->function registry
(define *shen-functions* (make-hash-table eq?))
(define *shen-function-arities* (make-hash-table eq?))

(define (register-function name function)
  (hash-table-set! *shen-functions* name function))

(define (register-function-arity name arity)
  (hash-table-set! *shen-function-arities* name arity))

(define (function-arity name)
  (if (symbol? name)
      (hash-table-ref/default *shen-function-arities* name -1)
      -1))

(define (initialize-arity-table entries)
  (if (null? entries)
      'done
      (let ((name (car entries))
            (arity (cadr entries)))
        (register-function-arity name arity)
        (initialize-arity-table (cddr entries)))))

(initialize-arity-table
 '(absvector 1 adjoin 2 and 2 append 2 arity 1 assoc 2 boolean? 1 cd 1 compile 3 concat 2 cons 2 cons? 1
   cn 2 declare 2 destroy 1 difference 2 do 2 element? 2 empty? 1 enable-type-theory 1 interror 2 eval 1
   eval-kl 1 explode 1 external 1 fail-if 2 fail 0 fix 2 findall 5 freeze 1 fst 1 gensym 1 get 3
   get-time 1 address-> 3 <-address 2 <-vector 2 > 2 >= 2 = 2 hd 1 hdv 1 hdstr 1 head 1 if 3 integer? 1
   intern 1 identical 4 inferences 0 input 1 input+ 2 implementation 0 intersection 2 it 0 kill 0 language 0
   length 1 lineread 1 load 1 < 2 <= 2 vector 1 macroexpand 1 map 2 mapcan 2 maxinferences 1 not 1 nth 2
   n->string 1 number? 1 occurs-check 1 occurrences 2 occurs-check 1 optimise 1 or 2 os 0 package 3 port 0
   porters 0 pos 2 print 1 profile 1 profile-results 1 pr 2 ps 1 preclude 1 preclude-all-but 1 protect 1
   address-> 3 put 4 reassemble 2 read-file-as-string 1 read-file 1 read 1 read-byte 1 read-from-string 1
   release 0 remove 2 reverse 1 set 2 simple-error 1 snd 1 specialise 1 spy 1 step 1 stinput 0 stoutput 0
   string->n 1 string->symbol 1 string? 1 strong-warning 1 subst 3 sum 1 symbol? 1 tail 1 tl 1 tc 1 tc? 0
   thaw 1 tlstr 1 track 1 trap-error 2 tuple? 1 type 2 return 3 undefmacro 1 unprofile 1 unify 4 unify! 4
   union 2 untrack 1 unspecialise 1 undefmacro 1 vector 1 vector-> 3 value 1 variable? 1 version 0 warn 1
   write-byte 2 write-to-file 2 y-or-n? 1 + 2 * 2 / 2 - 2 == 2 <e> 1 @p 2 @v 2 @s 2 preclude 1 include 1
   preclude-all-but 1 include-all-but 1 where 2))
