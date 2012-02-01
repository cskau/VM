(define x (cons 7 '()))

(define x (cons 4 x))

(cons '* x)

;;; expected result (* 4 7)
