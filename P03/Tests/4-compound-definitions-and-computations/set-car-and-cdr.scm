(define list
  (lambda xs
    xs))

(define x (cons 1 (cons 2 (cons 3 '()))))

(define y (cons 4 (cons 5 (cons 6 '()))))

(define z (cons 7 (cons 8 '())))

(begin
  (set-car! x 0)
  (set-cdr! y z)
  (list x y))

;;; expected result: ((0 2 3) (4 7 8))
