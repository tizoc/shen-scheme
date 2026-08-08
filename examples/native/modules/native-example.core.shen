(package native-example.core
 [add-ten]

(define increment
  X -> (+ X 1))

(define add-ten
  X -> (+ (increment X) 9))
)
