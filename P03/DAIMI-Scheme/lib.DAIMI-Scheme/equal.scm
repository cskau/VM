;;; lib.DAIMI-Scheme/equal.scm
;;; 

;;;;;;;;;;

(define equal?
  (lambda (x y)
    (if (eqv? x y)
	#t
	(cond
	  [(string? x)
	   (and (string? y)
		(string=? x y))]
	  [(pair? x)
	   (and (pair? y)
		(if (null? (cdr x))
		    (and (null? (cdr y))
			 (equal? (car x) (car y)))
		    (and (equal? (car x) (car y))
			 (equal? (cdr x) (cdr y)))))]
	  [(vector? x)
	   (and (vector? y)
		(let ([len (vector-length x)])
		  (and (= len (vector-length y))
		       (letrec ([traverse
				 (lambda (i)
				   (if (= i 0)
				       (equal? (vector-ref x 0)
					       (vector-ref y 0))
				       (and (equal? (vector-ref x i)
						    (vector-ref y i))
					    (traverse (- i 1)))))])
			 (traverse (- len 1))))))]
	  [else
	   #f]))))

(define alt-equal?
  (lambda (x y)
    (call/cc (lambda (k)
	       (letrec ([walk
			 (lambda (x y)
			   (if (eqv? x y)
			       #t
			       (cond
				 [(string? x)
				  (if (string? y)
				      (if (string=? x y)
					  #t
					  (k #f))
				      (k #f))]
				 [(pair? x)
				  (if (pair? y)
				      (if (null? (cdr x))
					  (if (null? (cdr y))
					      (walk (car x) (car y))
					      (k #f))
					  (if (null? (cdr y))
					      (k #f)
					      (begin
						(walk (car x) (car y))
						(walk (cdr x) (cdr y)))))
				      (k #f))]
				 [(vector? x)
				  (if (vector? y)
				      (let ([len (vector-length x)])
					(if (= len (vector-length y))
					    (letrec ([traverse
						      (lambda (i)
							(if (= i 0)
							    (walk (vector-ref x 0)
								  (vector-ref y 0))
							    (begin
							      (walk (vector-ref x i)
								    (vector-ref y i))
							      (traverse (- i 1)))))])
					      (traverse (- len 1)))
					    (k #f)))
				      (k #f))]
				 [else
				  (k #f)])))])
		 (walk x y))))))

;;;;;;;;;;

;;; end of "equal.scm"
