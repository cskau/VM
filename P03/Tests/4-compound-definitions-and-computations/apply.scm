(define list
  (lambda xs
    xs))

(vector (apply (lambda (x) x) (list 1))
	(apply + (list 10 20))
	(apply apply (list (lambda (x) x) (list 1))))

;;; expected result: #3(1 30 1)
