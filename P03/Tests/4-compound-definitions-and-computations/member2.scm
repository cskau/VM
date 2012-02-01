;;; member2.scm

;;;;;;;;;;

(define member-alt
  (lambda (x xs)
    (letrec ([walk (lambda (xs)
		     (cond
		       [(null? xs)
			#f]
		       [(eqv? x (car xs))
			#t]
		       [else
			(walk (cdr xs))]))])
      (walk xs))))

(vector (member-alt 'x '(a b c)) (member-alt 'x '(a b x c)))

;;;;;;;;;;

;;; expected result: #2(#f #t)
