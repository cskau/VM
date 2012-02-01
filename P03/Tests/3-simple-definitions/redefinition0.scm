(define mycar car)
(define car cdr)
(define list (lambda xs xs))
(list (mycar '(1 2 3))
      (car '(4 5 6)))

;;; expected result: (1 (5 6))
