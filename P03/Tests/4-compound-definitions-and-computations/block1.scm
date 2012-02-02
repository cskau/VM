(define list
  (lambda xs
    xs))

(define foo
  (lambda (x)
    (list (letrec ([fac
		    (lambda (x)
		      (if (= x 0)
			  1
			  (* x (fac (- x 1)))))])
	    (fac x))
	  (letrec ([bar
		    (lambda (n)
		      (quotient (* n (- n 1)) 2))])
	    (bar x)))))

(foo 5)

;;; expected result: (120 10)
