(define x
  (vector 2 3 4))

(define nothing
  (vector-set! x 2 "asdf"))

x

;;; expected result: #3(2 3 "asdf")
