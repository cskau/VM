(define list
  (lambda (xs)
    xs))

(define xs
  (cons 20 '()))

(define xs
  (cons 15 xs))

(define xs
  (cons 10 xs))

(define xs
  (cons 5 xs))

(define ys
  (cons xs '()))

(define zs
  (cons ys '()))

(define zs
  (cons list zs))

(apply apply zs)

;;; expected result: (5 10 15 20)
