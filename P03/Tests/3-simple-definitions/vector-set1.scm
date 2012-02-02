(define x
  (vector 2 3 4))

(define nothing
  (vector-set! x 0 5))

x

;;; expected result: #3(5 3 4)
