(define list
  (lambda xs
    xs))

(define xs
  (cons 20 '()))

(define xs
  (cons 15 xs))

(define xs
  (cons 10 xs))

(define xs
  (cons 5 xs))

(apply list xs)

;;; expected result: (5 10 15 20)
