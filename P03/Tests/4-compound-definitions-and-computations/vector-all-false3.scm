(vector (boolean? (lambda (x) x))
	(boolean? '(a b))
	(boolean? (cons 'a (cons 'b '())))
	(boolean? (vector 1 2 3))
	(boolean? (string #\a #\b #\c)))

;;; expected result: #5(#f)
