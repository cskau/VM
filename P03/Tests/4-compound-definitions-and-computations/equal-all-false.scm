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
	   (and (equal? (car x) (car y))
		(equal? (cdr x) (cdr y)))]
	  [else
	   #f]))))

(vector (equal? '() ''())
	(equal? 42 44)
	(equal? #t #f)
	(equal? "foo" "bar")
	(equal? 'bar 'baz)
	(equal? 'baz (string->symbol "buz"))
	(equal? car cdr)
	(equal? equal? (lambda (x y) (equal? x y)))
	(equal? '(a (b c) (((((d)))))) '(a (b c) (((((e)))))))
	(equal? (vector 1 2 3 4) (vector 1 2 3))
	(equal? (vector 1 2 3 4 5) (vector 1 2 3 4 6))
	(equal? "hello world" "hello world!"))

;;; expected result: #12(#f)

