\* Copyright (c) 2012-2026 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(define assert-kernel-equal
  Name Expected Expected -> (output "[OK]    ~A~%" Name)
  Name Expected Actual -> (error "~A: expected ~R, got ~R~%" Name Expected Actual))

(assert-kernel-equal
  "kernel version"
  "43.1"
  (version))

(assert-kernel-equal
  "public character stream arities"
  [1 1 2]
  (map (fn arity) [char-stream? read-unit-string write-string]))

(assert-kernel-equal
  "public character stream callable forms"
  [false unsupported unsupported]
  [((fn char-stream?) (stinput))
   (trap-error ((fn read-unit-string) (stinput)) (/. E unsupported))
   (trap-error ((fn write-string) "" (stoutput)) (/. E unsupported))])

(assert-kernel-equal
  "character stream API is exported and typed"
  [[true true] [true true] [true true]]
  (map (/. F [(element? F (external shen))
              (not (empty? (assoc F (value shen.*sigf*))))])
       [char-stream? read-unit-string write-string]))

(assert-kernel-equal
  "context macros splice list tails"
  [[cons a tail] [+ [cons a tail]]]
  [(macroexpand [shen.@c (cons a (cons (intern "bar!") (cons tail [])))])
   (macroexpand [shen.@ch [+ (cons a (cons (intern "bar!") (cons tail [])))]])])

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

(assert-kernel-equal
  "empty dictionary keys and values are lists"
  [[] []]
  (let Dict (shen.dict 4)
    [(shen.dict-keys Dict)
     (shen.dict-values Dict)]))

(assert-kernel-equal
  "dictionary traversal follows the kernel contract"
  [[answer] [42] [answer 42 seed]]
  (let Dict (shen.dict 4)
       Set (shen.dict-> Dict answer 42)
    [(shen.dict-keys Dict)
     (shen.dict-values Dict)
     (shen.dict-fold (/. K V Acc [K V Acc]) Dict seed)]))

(assert-kernel-equal
  "dictionary fold threads its accumulator"
  3
  (let Dict (shen.dict 4)
       First (shen.dict-> Dict one 1)
       Second (shen.dict-> Dict two 2)
    (shen.dict-fold (/. _ V Acc (+ V Acc)) Dict 0)))
