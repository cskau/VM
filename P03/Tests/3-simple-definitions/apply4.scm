(define plus
  (lambda (x y)
    (+ x y)))

(define xs
  (cons 10 '()))

(define xs
  (cons 5 xs))

(apply plus xs)

;;; expected result: 15
