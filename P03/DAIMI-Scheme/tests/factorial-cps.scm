(define foo
  (lambda (x)
    (letrec ([fac-c
	      (lambda (x k)
		(if (= x 0)
		    (k 1)
		    (fac-c (- x 1) (lambda (w)
				     (k (* w x))))))])
      (fac-c x (lambda (a) a)))))

(foo 5)

;;; expected answer: 120

