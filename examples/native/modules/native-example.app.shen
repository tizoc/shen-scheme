(package native-example.app
 (append [run-example module-events] (external native-example.core))

(set *events* [])

(define run-example
  X -> (add-ten X))

(set *events* [(run-example 32)])

(define module-events
  -> (value *events*))
)
