;;; member1.scm

;;;;;;;;;;

(define not
  (lambda (b)
    (if b
	#f
	#t)))

(define member
  (lambda (x xs)
    (letrec ([walk (lambda (xs)
		     (and (not (null? xs))
			  (or (eqv? x (car xs))
			      (walk (cdr xs)))))])
      (walk xs))))

(vector (member 'x '(a b c)) (member 'x '(a b x c)))

;;;;;;;;;;

;;; expected result: #2(#f #t)
