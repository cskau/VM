(define copy-list
  (lambda (xs)
    (letrec ([traverse (lambda (xs k)
			 (if (null? xs)
			     (k '())
			     (traverse (cdr xs) (lambda (ans)
						  (cons (car xs) (k ans))))))])
      (traverse xs (lambda (ans)
		     ans)))))

(copy-list '(1 2 3))

;;; what do you think?
