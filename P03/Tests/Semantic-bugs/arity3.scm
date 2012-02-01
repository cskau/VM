(((lambda (f)	;;; fixpoint operator (magic)
    ((lambda (a)
       (f (lambda (xs) ((a a) xs))))
     (lambda (a)
       (f (lambda (xs) ((a a) xs))))))
  (lambda (append)
    (lambda (x y)
      (if (null? x)
	  y
	  (cons (car x) (append (cdr x) y))))))
 '(1 2 3) '(4 5 6))

;;; arity mismatch
