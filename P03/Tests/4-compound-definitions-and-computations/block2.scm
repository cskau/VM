(define list
  (lambda xs
    xs))

(define foo
  (lambda (x)
    (let ([v1 (letrec ([fac
			(lambda (x)
			  (if (= x 0)
			      1
			      (* x (fac (- x 1)))))])
		(fac x))]
	  [v2 (letrec ([bar
			(lambda (n)
			  (quotient (* n (- n 1)) 2))])
		(bar x))])
      (list v1 v2))))

(foo 5)

;;; expected result: (120 10)
