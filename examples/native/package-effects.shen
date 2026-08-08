(package native-example.effects
 [effect-events]

(set *events* [])

(record "inside-before-definition")

(define record
  X -> (set *events* (append (value *events*) [X])))

(record "after-definition")

(define effect-events
  -> (value *events*))
)
