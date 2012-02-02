(define p (cons 1 2))

(define nothing
  (set-car! p 10))

(car p)

;;; expected result: 10

