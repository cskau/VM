(define x (cons 'b '()))

(define x (cons 'a x))

(cdr x)

;;; expected result (b)
