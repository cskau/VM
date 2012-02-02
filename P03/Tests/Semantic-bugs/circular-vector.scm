(define v (vector 0))

(begin
  (vector-set! v 0 v)
  v)

;;; expected result: a circular vector, ie, something not fit for print.
