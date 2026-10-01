(defmacro ns-test-identity
  [ns-test-identity Name] -> [define Name { (protect A) --> (protect A) }
                             (protect X) -> (protect X)])

(defmacro ns-test-inc
  [ns-test-inc X] -> [+ X 1])

(defmacro ns-test-container
  [ns-test-container] -> [shen.x.namespace ns-test.generated
                         [define answer -> 42]])

(defmacro ns-test-register
  [ns-test-register] -> (do (shen.register-source-form ns-test.temporary (/. F []))
                          [define ns-test.registered -> 1]))
