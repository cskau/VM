;;; lib.DAIMI-Scheme/list.scm
;;; 

;;; requires "cxxr.scm"

;;;;;;;;;;

(define list
  (lambda xs
    xs))

(define list?
  (lambda (v)
    (or (null? v)
	(pair? v))))

; (define proper-list?
;   (lambda (v)
;     (or (null? v)
;         (and (pair? v)
;              (proper-list? (cdr v))))))

(define proper-list?
  (lambda (xs)
    (letrec ([walk (lambda (xs1 xs2)
		     (or (null? xs2)
			 (and (pair? xs2)
			      (and (not (eqv? xs1 xs2))
				   (let ([xs2p (cdr xs2)])
				     (or (null? xs2p)
					 (and (pair? xs2p)
					      (walk (cdr xs1)
						    (cdr xs2p)))))))))])
      (or (null? xs)
	  (and (pair? xs)
	       (walk xs (cdr xs)))))))

(define test-proper-list?
  (lambda (range)
    (letrec ([outer
	      (lambda (head last-pair n)
		(or (> n range)
		    (letrec ([inner
			      (lambda (m)
				(if (eqv? (cdr last-pair) last-pair)
				    (begin
				      (set-cdr! last-pair
						(cons n '()))
				      (outer head
					     (cdr last-pair)
					     (+ n 1)))
				    (begin
				      (set-cdr! last-pair
						(cdr (cdr last-pair)))
				      (if (proper-list? head)
					  (error 'test-proper-list?
						 "length ~s, cycle ~s"
						 n
						 m)
					  (inner (+ m 1))))))])
		      (begin
			(set-cdr! last-pair head)
			(if (proper-list? head)
			    (error 'test-proper-list? "length ~s, cycle 0" n)
			    (inner 0))))))])
      (let ([head (cons 0 '())])
	(outer head head 1)))))

;;;;;;;;;;

(define append2
  (lambda (xs ys)
    (letrec ([walk (lambda (xs)
		     (if (null? xs)
			 ys
			 (cons (car xs) (walk (cdr xs)))))])
      (walk xs))))

(define append
  (lambda xs
    (case (length xs)
      [(0)
       '()]
      [(1)
       (car xs)]
      [(2)
       (append2 (car xs) (cadr xs))]
      [else
       (foldr append2 '() xs)])))

(define reverse
  (lambda (xs)
    (foldl cons '() xs)))

(define length
  (lambda (xs)
    (letrec ([loop (lambda (xs a)
		     (if (null? xs)
			 a
			 (loop (cdr xs) (+ a 1))))])
      (loop xs 0))))

(define last-pair
  (lambda (xs)
    (if (pair? xs)
	(letrec ([traverse (lambda (p xs)
			     (if (pair? xs)
				 (traverse xs (cdr xs))
				 p))])
	  (traverse xs (cdr xs)))
	(error 'last-pair "not a non-empty list: ~s" xs))))

;;;;;;;;;;

(define list-ref
  (lambda (xs i)
    (if (< i 0)
	(error 'list-ref "negative offset: ~s" i)
	(letrec ([loop
		  (lambda (ys j)
		    (cond
		      [(null? ys)
		       (error 'list-ref
			      "offset ~s is out of range for ~s" i xs)]
		      [(= i j)
		       (car ys)]
		      [else
		       (loop (cdr ys) (+ j 1))]))])
	  (loop xs 0)))))

(define ref-list
  (lambda (xs x)
    (letrec ([loop
	      (lambda (ys offset)
		(cond
		  [(null? ys)
		   (error 'ref-list "~s is not in ~s" x xs)]
		  [(equal? (car ys) x)
		   offset]
		  [else
		   (loop (cdr ys) (+ offset 1))]))])
      (loop xs 0))))

(define list-set!
  (lambda (xs i v)
    (if (< i 0)
	(error 'list-set! "negative offset: ~s" i)
	(letrec ([loop
		  (lambda (ys j)
		    (cond
		      [(null? ys)
		       (error 'list-set!
			      "offset ~s is out of range for ~s" i xs)]
		      [(= i j)
		       (let ([res (car ys)])
			 (begin
			   (set-car! ys v)
			   res))]
		      [else
		       (loop (cdr ys) (+ j 1))]))])
	  (loop xs 0)))))

;;;;;;;;;;

;;; end of "list.scm"
