\* Exercise observable evaluation order, including errors and intermediate
   curried applications. Run against the rebuilt runtime as well as the
   compiler's explicit Scheme output. *\

(set _scm.*compiling-shen-sources* false)

(define compiler-order-mark
  Label X -> (do (set *compiler-order-trace*
                     (append (value *compiler-order-trace*) [Label]))
                 X))

(define compiler-order-next
  -> (let C (value *compiler-order-counter*)
          _ (set *compiler-order-counter* (+ C 1))
       (compiler-order-mark C C)))

(define compiler-order-run
  F -> (let _ (set *compiler-order-counter* 0)
            _ (set *compiler-order-trace* [])
            Result (thaw F)
         [Result (value *compiler-order-trace*)]))

(define compiler-order-three A B C -> [A B C])

(assert-equal
 (compiler-order-run (freeze [(compiler-order-next) (compiler-order-next)]))
 [[0 1] [0 1]])

(assert-equal
 (compiler-order-run
  (freeze [(compiler-order-next) (compiler-order-next) (compiler-order-next)]))
 [[0 1 2] [0 1 2]])

(assert-equal
 (compiler-order-run
  (freeze [(compiler-order-next) (compiler-order-next) (compiler-order-next)
           (compiler-order-next) (compiler-order-next)]))
 [[0 1 2 3 4] [0 1 2 3 4]])

(assert-equal
 (compiler-order-run
  (freeze (compiler-order-three
           (compiler-order-next) (compiler-order-next) (compiler-order-next))))
 [[0 1 2] [0 1 2]])

(assert-equal
 (compiler-order-run
  (freeze (- (compiler-order-next) (compiler-order-next))))
 [-1 [0 1]])

(assert-equal
 (compiler-order-run
  (freeze (cons (compiler-order-mark head 1) (compiler-order-mark tail []))))
 [[1] [head tail]])

(assert-equal
 (compiler-order-run
  (freeze (= (compiler-order-mark left 1) (compiler-order-mark right 1))))
 [true [left right]])

(assert-equal
 (compiler-order-run
  (freeze (pos (compiler-order-mark string "abc") (compiler-order-mark index 1))))
 ["b" [string index]])

(assert-equal
 (compiler-order-run
  (freeze (let V (absvector 1)
               _ (address-> (compiler-order-mark vector V)
                            (compiler-order-mark index 0)
                            (compiler-order-mark element 42))
            (<-address V 0))))
 [42 [vector index element]])

(assert-equal
 (compiler-order-run
  (freeze (let V (absvector 1)
               _ (address-> V 0 42)
            (<-address (compiler-order-mark vector V)
                       (compiler-order-mark index 0)))))
 [42 [vector index]])

(assert-equal
 (compiler-order-run
  (freeze ((compiler-order-mark operator (/. X X))
           (compiler-order-mark argument 7))))
 [7 [operator argument]])

(assert-equal
 (compiler-order-run
  (freeze (let F (/. A (do (compiler-order-mark applied-first true)
                          (/. B [A B])))
            (F (compiler-order-mark first 1) (compiler-order-mark second 2)))))
 [[1 2] [first applied-first second]])

(assert-equal
 (compiler-order-run
  (freeze (trap-error
           (let F (/. A (do (compiler-order-mark applied-first true)
                            (simple-error "stop")))
             (F (compiler-order-mark first 1) (compiler-order-mark second 2)))
           (/. E caught))))
 [caught [first applied-first]])

(assert-equal
 (compiler-order-run
  (freeze (trap-error
           (compiler-order-three
            (do (compiler-order-mark first 1) (simple-error "stop"))
            (compiler-order-mark second 2)
            (compiler-order-mark third 3))
           (/. E caught))))
 [caught [first]])

\* Scheme escapes can name arbitrary syntax and retain Scheme semantics. *\
(assert-equal
 (compiler-order-run
  (freeze (eval-kl [scm.and [compiler-order-mark first false]
                           [compiler-order-mark second true]])))
 [false [first]])

(assert-equal
 (compiler-order-run
  (freeze (eval-kl [scm.or [compiler-order-mark first true]
                          [compiler-order-mark second false]])))
 [true [first]])

(assert-equal
 (compiler-order-run
  (freeze (eval-kl [scm.begin [compiler-order-mark first 1]
                             [compiler-order-mark second 2]])))
 [2 [first second]])

(assert-equal
 (compiler-order-run
  (freeze (eval-kl [scm.if false [compiler-order-mark then 1]
                                [compiler-order-mark else 2]])))
 [2 [else]])

(assert-equal
 (compiler-order-run
  (freeze (do (eval-kl [scm.when false [compiler-order-mark first 1]
                                      [compiler-order-mark second 2]])
              done)))
 [done []])

(define compiler-order-eval-scheme
  Form -> ((eval-kl [lambda x [scm.eval x]]) Form))

(define compiler-order-native-call
  Mode -> (compiler-order-eval-scheme
           (_scm.with-native-context
            Mode [[compiler-order-three (intern "kl:compiler-order-three")]]
            (freeze (_scm.kl->scheme
                     [compiler-order-three [compiler-order-next]
                                           [compiler-order-next]
                                           [compiler-order-next]])))))

(assert-equal
 (compiler-order-run (freeze (compiler-order-native-call sealed)))
 [[0 1 2] [0 1 2]])

(assert-equal
 (compiler-order-run (freeze (compiler-order-native-call app)))
 [[0 1 2] [0 1 2]])

\* Compiler-generated Scheme helpers still implement Shen operand order. *\
(assert-equal
 (compiler-order-run
  (freeze
   (let Saved (value _scm.*compiling-shen-sources*)
        _ (set _scm.*compiling-shen-sources* true)
        Result (eval-kl
                [trap-error
                 [get [compiler-order-mark object compiler-order-missing]
                      [compiler-order-mark property compiler-order-property]
                      [do [compiler-order-mark dictionary 0]
                          [value *property-vector*]]]
                 [lambda e missing]])
        _ (set _scm.*compiling-shen-sources* Saved)
     Result)))
 [missing [object property dictionary]])

\* A generated binding must not capture a name already present in the KL. *\
(assert-equal
 (compiler-order-run
  (freeze
   (let KL [let _scm.arg1 99
             [cons [compiler-order-next]
               [cons _scm.arg1 [cons [compiler-order-next] []]]]]
        Saved (value shen.*gensym*)
        _ (set shen.*gensym* 0)
        Scheme (_scm.kl->scheme KL)
        _ (set shen.*gensym* Saved)
     (compiler-order-eval-scheme Scheme))))
 [[0 99 1] [0 1]])
