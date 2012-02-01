(define v (make-vector 3 24))

(begin
  (vector-set! v 2 100)
  v)

;;; expected result: #3(24 24 100)
