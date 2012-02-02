(define vector->list
  (lambda (v)
    (let ([len (vector-length v)])
      (letrec ([walk (lambda (n)
		       (if (= n len)
			   '()
			   (cons (vector-ref v n)
				 (walk (+ 1 n)))))])
	(walk 0)))))

(vector->list (vector (cons 1 '())
		      (cons 1 '(2 3))
		      (car '(a b c))
		      (cdr '(a b c))))

;;; expected result: ((1) (1 2 3) a (b c))

