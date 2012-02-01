(define append2-alt
  (lambda (xs ys)
    (letrec ([walk (lambda (xs)
		     (if (null? xs)
			 ys
			 (cons (car xs) (walk (cdr xs)))))])
      (walk xs))))

(append2-alt '(1 2 3) '(4 5 6))

;;; expected result: (1 2 3 4 5 6)
