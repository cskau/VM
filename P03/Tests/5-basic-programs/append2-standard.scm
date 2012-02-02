(define append2
  (lambda (xs ys)
    (if (null? xs)
        ys
	(cons (car xs) (append2 (cdr xs) ys)))))

(append2 '(1 2 3) '(4 5 6))

;;; expected result: (1 2 3 4 5 6)
