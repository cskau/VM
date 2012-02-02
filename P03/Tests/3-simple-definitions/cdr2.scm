(define x (cons 4 '()))

(define x (cons 3 x))

(define x (cons '* x))

(cdr x)

;;; expected result (3 4)
