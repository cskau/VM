(define equal?
  (lambda (x y)
    (or (eqv? x y)
	(cond
	  [(and (integer? x) (integer? y))
	   (= x y)]
	  [(and (char? x) (char? y))
	   (= (char->integer x) (char->integer y))]
	  [(and (string? x) (string? y))
	   (let ([len (string-length x)])
	     (and (= len (string-length y))
		  (letrec ([walk
			    (lambda (i)
			      (or (= i len)
				  (and (equal? (string-ref x i)
					       (string-ref y i))
				       (walk (+ i 1)))))])
		    (walk 0))))]
	  [(and (vector? x) (vector? y))
	   (let ([len (vector-length x)])
	     (and (= len (vector-length y))
		  (letrec ([walk
			    (lambda (i)
			      (or (= i len)
				  (and (equal? (vector-ref x i)
					       (vector-ref y i))
				       (walk (+ i 1)))))])
		    (walk 0))))]
	  [(and (pair? x) (pair? y))
	   (if (and (null? (cdr x)) (null? (cdr y)))
	       (equal? (car x) (car y))
	       (and (equal? (car x) (car y))
		    (equal? (cdr x) (cdr y))))]
	  [else
	   #f]))))

(equal? (vector 1 (cons 2 (vector 3 4 #\a)))
	(vector 1 (cons 2 (vector 3 4 #\a))))

;;; expected result: #t

