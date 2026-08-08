(package native-example.binding
 [call-helper]

(define helper
  X -> (+ X 1))

(define call-helper
  X -> (helper X))
)
