;;; This program outputs the source definition of the factorial function, in Scheme.

(define list
  (lambda xs
    xs))

(define make-factorial
  (lambda (n)
    (list 'define
	  'factorial
	  (list 'lambda
		(list n)
		(list 'if
		      (list 'zero? n)
		      1
		      (list '* n (list 'factorial (list '- n 1))))))))

(make-factorial 'x)

;;; expected result: (define factorial (lambda (x) (if (zero? x) 1 (* x (factorial (- x 1))))))
