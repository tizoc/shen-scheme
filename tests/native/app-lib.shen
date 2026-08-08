(set *native-app-init-events* [])

(define native-app-helper
  X -> (+ X 1))

(set *native-app-init-events*
     (append (value *native-app-init-events*)
             [(native-app-helper 0)]))

(define native-app-hop
  X -> (native-app-helper X))
