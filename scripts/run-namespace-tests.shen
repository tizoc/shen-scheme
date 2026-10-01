(define ns-test.assert
  Label Expected Expected -> (output "[OK]    ~A~%" Label)
  Label Expected Actual -> (error "~A: expected ~R, got ~R~%" Label Expected Actual))

(define ns-test.check-model
  -> (do (ns-test.assert "namespace forward reference" 42 (eval [ns-test.model.caller 40]))
         (ns-test.assert "namespace macro definition" 42 (eval [ns-test.model.identity 42]))
         (ns-test.assert "namespace external data" ns-test-tag (eval [ns-test.model.tag 0]))))

(define ns-test.check-client
  -> (do (ns-test.assert "namespace alias" 42 (eval [ns-test.client.answer 40]))
         (ns-test.assert "namespace scoped macro" 42 (eval [ns-test.client.scoped 41]))
         (ns-test.assert "namespace alias as data" ns-test.model.caller
                         (eval [ns-test.client.reference 0]))))

(ns-test.assert "namespaces initialized at startup" true
  (cons? (shen.source-form-handler [shen.x.namespace] (value shen.*source-form-handlers*))))
(load "tests/namespaces/macros.shen")
(set ns-test-events [])
(load "tests/namespaces/model.shen")
(load "tests/namespaces/client.shen")
(ns-test.check-model)
(ns-test.check-client)

\* Compile after shadowing the dynamic functions to verify native definitions. *\
(define ns-test.model.caller X -> stale)
(define ns-test.model.identity X -> stale)
(set ns-test-events [])
(shen-scheme.compile-file "tests/namespaces/model.shen" "_build/namespace-tests/model.so")
(ns-test.assert "native namespace effects are deferred" [] (value ns-test-events))
(put ns-test.model shen.internal-symbols [])
(put ns-test.model shen.external-symbols [])
(shen-scheme.load-compiled "_build/namespace-tests/model.so")
(ns-test.check-model)
(ns-test.assert "native namespace effects run once" [ns-test.model.model] (value ns-test-events))
(ns-test.assert "native namespace internal metadata" true
  (element? ns-test.model.caller (internal ns-test.model)))
(ns-test.assert "native namespace external metadata" true
  (element? ns-test-tag (external ns-test.model)))

(define ns-test.client.answer X -> stale)
(shen-scheme.compile-file/mode "tests/namespaces/client.shen" "_build/namespace-tests/client.so" sealed)
(shen-scheme.load-compiled "_build/namespace-tests/client.so")
(ns-test.check-client)
(shen-scheme.compile-file "tests/namespaces/generated.shen" "_build/namespace-tests/generated.so")
(shen-scheme.load-compiled "_build/namespace-tests/generated.so")
(ns-test.assert "macro-generated native namespace" 42 (eval [ns-test.generated.answer]))

(shen.register-source-form ns-test.group (/. F (tl F)))
(shen-scheme.compile-file "tests/namespaces/group.shen" "_build/namespace-tests/group.so")
(shen-scheme.load-compiled "_build/namespace-tests/group.so")
(ns-test.assert "native source sequence and empty expansion" 42 (eval [ns-test.group-answer]))
(shen.unregister-source-form ns-test.group)

(let Before (value shen.*source-form-handlers*)
  (do (shen-scheme.compile-file "tests/namespaces/register.shen" "_build/namespace-tests/register.so")
      (ns-test.assert "successful native build restores source handlers"
                      Before (value shen.*source-form-handlers*))
      (ns-test.assert "invalid namespace aborts native compilation" failed
        (trap-error (shen-scheme.compile-file "tests/namespaces/error.shen"
                                              "_build/namespace-tests/error.so")
                    (/. E failed)))
      (ns-test.assert "failed native build restores source handlers"
                      Before (value shen.*source-form-handlers*))))

(set ns-test-events [])
(shen-scheme.compile-module "tests/namespaces/ns-test.model.shenmod"
                            "_build/namespace-tests/ns-test.model.so")
(shen-scheme.compile-module/in-dir "tests/namespaces/ns-test.client.shenmod"
                                   "_build/namespace-tests/ns-test.client.so" "tests/namespaces")
(ns-test.assert "namespace module analysis defers effects" [] (value ns-test-events))
(shen-scheme.load-module "tests/namespaces/ns-test.client.shenmod"
                        "tests/namespaces" "_build/namespace-tests")
(ns-test.check-client)

(shen-scheme.compile-module "tests/namespaces/ns-test.typed.shenmod"
                            "_build/namespace-tests/typed.so")
(shen-scheme.load-compiled "_build/namespace-tests/typed.so")
(ns-test.assert "typechecked native namespace forward reference" 42 (eval [ns-test.typed.answer 40]))

(shen-scheme.build-app "tests/namespaces/client.shen" ["tests/namespaces/model.shen"]
                       "_build/namespace-tests/app.so")
(shen-scheme.load-compiled "_build/namespace-tests/app.so")
(ns-test.check-client)
(shen-scheme.build-module-app/wpo "tests/namespaces/ns-test.client.shenmod" "tests/namespaces"
                                  "_build/namespace-tests/module-app.so")
(shen-scheme.load-compiled "_build/namespace-tests/module-app.so")
(ns-test.check-client)
