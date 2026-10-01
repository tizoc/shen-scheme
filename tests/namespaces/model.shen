(shen.x.namespace ns-test.model
  (externals [ns-test-identity ns-test-tag ns-test-events])
  (define caller {number --> number} X -> (later X 2))
  (define later {number --> number --> number} X Y -> (+ X Y))
  (define tag {number --> symbol} X -> ns-test-tag)
  (ns-test-identity identity)
  (set ns-test-events (cons model (value ns-test-events))))
