(define p (cons 1 2))

(define nothing
  (set-cdr! p 20))

(cdr p)

;;; expected result: 20

