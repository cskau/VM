(define two (car '(2 3 4)))
(define constant
  (lambda ()
    '(a b c)))
(and (= two 2)
     (pair? (cdr (constant)))
     (pair? (cdr (cdr (constant))))
     (null? (cdr (cdr (cdr (constant))))))

;;; expected result: #t
