(define identity
  (lambda (x)
    x))

(define xs
  (cons 10 '()))

(apply identity xs)

;;; expected result: 10
