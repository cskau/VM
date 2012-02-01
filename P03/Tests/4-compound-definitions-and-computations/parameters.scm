
(define a 0)
(define b (cons 1 '()))
(define c (cons 2 '()))

(let ([f (lambda (x) (set! x 3))]
      [g (lambda (x) (set! x 4))]
      [h (lambda (x) (set-car! x 5))])
  (begin
    (f a)
    (g b)
    (h c)
    (vector a b c)))

;;; Expected result: #3(0 (1) (5))
