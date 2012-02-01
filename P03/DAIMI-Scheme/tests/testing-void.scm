(define x 1)

(define update_x
  (lambda (y)
    (set! x y)))

(if (update_x 2) 10 20)

;;; expected result: 10
