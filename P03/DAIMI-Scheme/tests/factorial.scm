(define foo
  (lambda (x)
    (letrec ([fac
	      (lambda (x)
		(if (= x 0)
		    1
		    (* x (fac (- x 1)))))])
      (fac x))))

(foo 5)

;;; expected answer: 120

