(define ppm-test.match-simple
  [ppm-test.two A B] -> [A B]
  _ -> no)

(define ppm-test.match-repeat
  [ppm-test.two X X] -> same
  [ppm-test.two _ _] -> different)

(define ppm-test.match-nested
  [ppm-test.two [ppm-test.two A B] C] -> [A B C]
  _ -> no)

(define ppm-test.match-cons
  [H | T] -> [H T]
  _ -> no)
