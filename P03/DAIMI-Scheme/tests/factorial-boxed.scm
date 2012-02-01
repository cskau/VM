(define foo (lambda (x%0) (letrec ([fac%1 (lambda (x%2) (if (= x%2 0) 1 (* x%2 (fac%1 (- x%2 1)))))]) (fac%1 x%0))))
(foo 5)
