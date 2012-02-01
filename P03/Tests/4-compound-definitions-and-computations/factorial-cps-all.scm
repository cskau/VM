(define foo
  (lambda (x)
    (letrec ([fac-c
	      (lambda (x k)
		(zero-c x
			(lambda (b)
			  (if b
			      (k 1)
			      (sub1-c x
				      (lambda (v)
					(fac-c v
					       (lambda (w)
						 (*-c w x k)))))))))]
	     [zero-c
	      (lambda (n k)
		(k (= n 0)))]
	     [sub1-c
	      (lambda (n k)
		(k (- n 1)))]
	     [*-c
	      (lambda (x y k)
		(k (* x y)))])
      (fac-c x (lambda (a) a)))))

(foo 5)

;;; expected result: 120

