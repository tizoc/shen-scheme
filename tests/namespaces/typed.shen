(shen.x.namespace ns-test.typed
  (define answer {number --> number} X -> (later X 2))
  (define later {number --> number --> number} X Y -> (+ X Y)))
