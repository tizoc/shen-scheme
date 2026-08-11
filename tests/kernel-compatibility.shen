\* Copyright (c) 2012-2026 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(define assert-kernel-equal
  Name Expected Expected -> (output "[OK]    ~A~%" Name)
  Name Expected Actual -> (error "~A: expected ~R, got ~R~%" Name Expected Actual))

(assert-kernel-equal
  "input+ macro expansion"
  [shen.input-h+ number [stinput]]
  (macroexpand [input+ number]))

(assert-kernel-equal
  "input+ direct and higher-order calls"
  [41 42]
  (let File "_build/input-plus-regression.shen"
       Written (write-to-file File "41 42")
       Stream (open File in)
       Direct (input+ number Stream)
       HigherOrder ((fn input+) number Stream)
       Closed (close Stream)
       Deleted (delete-file File)
       [Direct HigherOrder]))
