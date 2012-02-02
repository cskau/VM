(define downfrom
  (lambda (n)
    (if (< n 0)
	'()
	(cons n (downfrom (- n 1))))))

(downfrom 5)

;;; expected result: (5 4 3 2 1 0)
