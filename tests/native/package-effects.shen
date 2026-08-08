(set *native-package-events* [])

(package native.test.pkg
  [native-package-main native-package-state *native-package-events*]

(defmacro twice
  [twice X] -> [* X 2])

(synonyms amount number)

(declare helper [amount --> amount])

(define helper
  X -> (+ X 1))

(define remember
  X -> (set *native-package-events*
            (append (value *native-package-events*) [X])))

(remember (helper 40))

(define native-package-main
  X -> (twice (helper X)))

(define native-package-state
  -> (value *native-package-events*)))

(set *native-package-events*
     (append (value *native-package-events*) [2]))
