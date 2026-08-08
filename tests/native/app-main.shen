(set *native-app-init-events*
     (append (value *native-app-init-events*)
             [(native-app-main 1)]))

(define native-app-main
  X -> (+ (native-app-hop X) 10))

(define native-app-direct
  X -> (native-app-helper X))

(define native-app-length
  Xs -> (length Xs))

(define native-app-absvector?
  -> (absvector? (absvector 1)))

(define native-app-list-equal?
  -> (= [a list] [a list]))

(define native-app-sysfunc?
  -> (shen.sysfunc? cons))
