(shen.x.namespace ns-test.client
  (use ns-test.model => model)
  (define answer {number --> number} X -> (model.caller X))
  (define scoped {number --> number} X -> (with-externals [ns-test-inc]
                       (ns-test-inc (model.identity X))))
  (define reference X -> model.caller))
