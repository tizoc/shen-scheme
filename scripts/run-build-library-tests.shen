\* Copyright (c) 2012-2021 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(load "scripts/build.shen")

(define build-library-test.assert-equal
  Label Expected Expected -> (pr (make-string "[OK]    ~A~%" Label))
  Label Expected Actual -> (error "build library test failed: ~A expected ~R, got ~R~%"
                                  Label Expected Actual))

(define build-library-test.assert-contains
  Label Needle String -> (build-library-test.assert-equal
                          Label
                          true
                          (string.infix? Needle String)))

(define build-library-test.run
  -> (let Public "_build/shen-library-test.ss"
          Runtime "_build/shen-runtime-library-test.ss"
       (do
         (shen-scheme.delete-file-if-exists Public)
         (shen-scheme.delete-file-if-exists Runtime)
         (set *runtime-library-file* Runtime)
         (build library Public)
         (let PublicBody (read-file-as-string Public)
              RuntimeBody (read-file-as-string Runtime)
           (do
             (build-library-test.assert-contains
              "library mode emits public Shen library"
              "(library (shen)"
              PublicBody)
             (build-library-test.assert-contains
              "public library exports initialize-shen"
              "(export initialize-shen"
              PublicBody)
             (build-library-test.assert-contains
              "public library imports the runtime"
              "(import (shen-scheme runtime))"
              PublicBody)
             (build-library-test.assert-contains
              "public library preserves quiet-load"
              "(define kl:shen.quiet-load kl:shen.x.launcher.quiet-load)"
              PublicBody)
             (build-library-test.assert-contains
              "public library preserves run-shen"
              "(define kl:shen.run-shen kl:shen-scheme.run-shen)"
              PublicBody)
             (build-library-test.assert-contains
              "library mode emits companion runtime library"
              "(library (shen-scheme runtime)"
              RuntimeBody))))))

(build-library-test.run)
