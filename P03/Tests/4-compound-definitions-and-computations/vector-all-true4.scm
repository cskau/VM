(vector (null? '())
	(null? (cdr (cons 1 '())))
	(null? (car '(())))
	(null? (vector-ref (vector '()) 0)))

;;; expected result: #4(#t)
