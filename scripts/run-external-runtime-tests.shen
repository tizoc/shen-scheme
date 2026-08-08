\* Copyright (c) 2012-2021 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(define external-runtime-test.assert-equal
  L X X -> (pr (make-string "[OK]    ~A~%" L))
  L X Y -> (error "external runtime test failed: ~A expected ~R, got ~R~%"
                  L X Y))

(define external-runtime-test.assert-app
  O -> (do
         (shen-scheme.load-compiled O)
         (external-runtime-test.assert-equal
          "external runtime runs app initializers"
          [1 12]
          (value *native-app-init-events*))
         (external-runtime-test.assert-equal
          "external runtime calls app entry point"
          42
          (eval [native-app-main 31]))
         (external-runtime-test.assert-equal
          "external runtime calls app helper"
          42
          (eval [native-app-direct 41]))
         (external-runtime-test.assert-equal
          "external runtime calls runtime global"
          3
          (eval [native-app-length [cons 1 [cons 2 [cons 3 []]]]]))
         (external-runtime-test.assert-equal
          "external runtime uses absvector fallback"
          true
          (eval [native-app-absvector?]))
         (external-runtime-test.assert-equal
          "external runtime uses generic equality fallback"
          true
          (eval [native-app-list-equal?]))
         (external-runtime-test.assert-equal
          "external runtime uses static runtime global"
          true
          (eval [native-app-sysfunc?]))))

(define external-runtime-test.full
  App O -> (do
             (external-runtime-test.assert-app App)
             (shen-scheme.delete-file-if-exists O)
             (shen-scheme.compile-file "tests/native/simple.shen" O)
             (external-runtime-test.assert-equal
              "full external runtime emits native object"
              true
              (shen-scheme.file-exists? O))
             (shen-scheme.load-compiled O)
             (external-runtime-test.assert-equal
              "full external runtime compiles and loads native source"
              [5 6]
              (eval [native-test-map-inc
                     [cons 4 [cons 5 []]]]))))

(define external-runtime-test.petite
  App O -> (let R (do
                    (external-runtime-test.assert-app App)
                    (shen-scheme.delete-file-if-exists O)
                    (trap-error
                     (do
                       (shen-scheme.compile-file "tests/native/simple.shen" O)
                       unexpected-success)
                     (/. E (error-to-string E))))
            (do
              (external-runtime-test.assert-equal
               "Petite external runtime has no compiler"
               "compile package is not loaded"
               R)
              (external-runtime-test.assert-equal
               "Petite external runtime emits no native object"
               false
               (shen-scheme.file-exists? O)))))

(define external-runtime-test.run
  [_ "full" App O] -> (external-runtime-test.full App O)
  [_ "petite" App O] -> (external-runtime-test.petite App O)
  X -> (error
        "usage: run-external-runtime-tests.shen full|petite APP OBJECT; got ~S~%"
        X))

(external-runtime-test.run (value *argv*))
