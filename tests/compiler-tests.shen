\* Copyright (c) 2012-2021 Bruno Deferrari.  All rights reserved.    *\
\* BSD 3-Clause License: http://opensource.org/licenses/BSD-3-Clause *\

(load "src/compiler.shen")

(assert-equal
 (_scm.force-boolean true)
 true)

(assert-equal
 (_scm.force-boolean false) false)

(assert-equal
 (_scm.force-boolean [number? 1])
 [number? 1])

(assert-equal
 (_scm.force-boolean [+ 1 2])
 [_scm.assert-boolean [+ 1 2]])

(assert-equal
 (_scm.force-boolean [let X 1 [number? X]])
 [let X 1 [number? X]])

(assert-equal
 (_scm.force-boolean [let X 1 [+ X X]])
 [_scm.assert-boolean [let X 1 [+ X X]]])

(assert-equal
 (_scm.force-boolean [and true false])
 [and true false])

(assert-equal
 (_scm.prefix-op test)
 (intern "kl:test"))

\\ compile-expression

(assert-equal
 (_scm.kl->scheme [])
 [quote []])

(assert-equal
 (_scm.kl->scheme true)
 true)

(assert-equal
 (_scm.kl->scheme false)
 false)

(assert-equal
 (_scm.kl->scheme {)
 [quote {])

(assert-equal
 (_scm.kl->scheme })
 [quote }])

(assert-equal
 (_scm.kl->scheme ;)
 [quote ;])

(assert-equal
 (_scm.kl->scheme ,)
 [quote ,])

(assert-equal
 (_scm.kl->scheme some-symbol)
 [quote some-symbol])

(assert-equal
 (_scm.kl->scheme [let A 1 [+ A A]])
 [let [[A 1]] [+ A A]])

(assert-equal
 (_scm.kl->scheme [lambda X [= X 1]])
 [lambda [X] [eqv? X 1]])

(assert-equal
 (_scm.compile-expression [and [some-func X] [= 1 2]] [X])
 [and [(_scm.prefix-op _scm.assert-boolean) [(_scm.prefix-op some-func) X]] [eqv? 1 2]])

(assert-equal
 (_scm.compile-expression [or [some-func X] [= 1 2]] [X])
 [or [(_scm.prefix-op _scm.assert-boolean) [(_scm.prefix-op some-func) X]] [eqv? 1 2]])

(assert-equal
 (_scm.kl->scheme [trap-error [+ 1 2] [lambda E 0]])
 [guard [E [else 0]] [+ 1 2]])

(assert-equal
 (_scm.kl->scheme [do 1 2])
 [begin 1 2])

(assert-equal
 (_scm.kl->scheme [freeze [print "hello"]])
 [lambda [] [(_scm.prefix-op print) "hello"]])

(assert-equal
 (_scm.kl->scheme [fail])
 [(_scm.prefix-op fail)])

(assert-equal
 (_scm.kl->scheme [blah 1 2])
 [(_scm.prefix-op blah) 1 2])

(assert-equal
 (_scm.kl->scheme 1)
 1)

(assert-equal
 (_scm.kl->scheme "string")
 "string")

(assert-equal
 (_scm.kl->scheme [defun some-name [A B C] [cons symbol [+ A B]]])
 [define [(_scm.prefix-op some-name) A B C] [cons [quote symbol] [+ A B]]])

(assert-equal
 (_scm.compile-expression [F 1 2 3] [F])
 [[[F 1] 2] 3])


(define takes-0-args -> 0)

(assert-equal
 (_scm.kl->scheme [takes-0-args])
 [(_scm.prefix-op takes-0-args)])

(assert-equal
 (_scm.kl->scheme [takes-?-args])
 [(_scm.prefix-op takes-?-args)])

(assert-equal
 (_scm.kl->scheme [takes-?-args 1 2 3])
 [(_scm.prefix-op takes-?-args) 1 2 3])

(set _scm.*compiling-shen-sources* true)

(define default D E -> D)

(assert-equal
 (_scm.kl->scheme [trap-error [value varname] [lambda E default]])
 (_scm.kl->scheme [scm.value/or varname [freeze default]]))

(assert-equal
 (_scm.kl->scheme [trap-error [<-address Var [+ 10 10]] [lambda E default]])
 (_scm.kl->scheme [scm.<-address/or Var [+ 10 10] [freeze default]]))

(assert-equal
 (_scm.kl->scheme [trap-error [<-vector Var [+ 10 10]] [lambda E default]])
 (_scm.kl->scheme [scm.<-vector/or Var [+ 10 10] [freeze default]]))

(assert-equal
 (_scm.kl->scheme [trap-error [get Var prop Dict] [lambda E default]])
 (_scm.kl->scheme [scm.get/or Var prop Dict [freeze default]]))

(assert-equal
  (_scm.kl->scheme [scm. "(+ 1 2)"])
  [+ 1 2])

(assert-equal
  (_scm.kl->scheme [scm. "symbol"])
  symbol)

(assert-equal
  (_scm.kl->scheme [scm. "(lambda () 1)"])
  [lambda [] 1])


(assert-equal
  (_scm.kl->scheme [scm.letrec [[X 1] [Y 2]] [+ X Y]])
  [letrec [[X 1] [Y 2]] [+ X Y]])
