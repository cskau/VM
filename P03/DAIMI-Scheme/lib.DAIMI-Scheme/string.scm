;;; lib.DAIMI-Scheme/string.scm
;;; 

;;; requires "list.scm"

;;;;;;;;;;

; (define string-append
;   (lambda strings
;     (if (andmap1 string? strings)
;         (foldr string-append "" strings)
;         (error 'string-append
;                "non-string elements: ~s" (filter-out string? strings)))))

; (define string=?
;   (lambda (s1 s2)
;     (let ([len (string-length s1)])
;       (and (= len (string-length s2))
;            (letrec ([loop
;                      (lambda (offset)
;                        (or (= offset len)
;                            (and (char=? (string-ref s1 offset)
;                                         (string-ref s2 offset))
;                                 (loop (+ offset 1)))))])
;              (loop 0))))))

(define string<?
  (lambda (s1 s2)
    (let ([l1 (string-length s1)]
	  [l2 (string-length s2)])
      (letrec ([loop
		(lambda (i1 i2)
		  (cond
		    [(= i2 l2)
		     #f]
		    [(= i1 l1)
		     #t]
		    [else
		     (let ([j1 (char->integer (string-ref s1 i1))]
			   [j2 (char->integer (string-ref s2 i2))])
		       (cond
			 [(< j1 j2)
			  #t]
			 [(> j1 j2)
			  #f]
			 [else
			  (loop (+ i1 1) (+ i2 1))]))]))])
	(loop 0 0)))))

(define substring
  (lambda (s i j)
    (letrec ([walk (lambda (i)
		     (if (>= i j)
			 '()
			 (cons (string-ref s i)
			       (walk (+ i 1)))))])
      (if (and (<= 0 i) (<= i j) (<= j (string-length s)))
	  (apply string (walk i))
	  (error 'substring
		 "~s and ~s are not valid start/end indices for ~s"
		 i j s)))))

(define string-suffix?
  (lambda (text suffix)
    (cond
      [(not (string? text))
       (error 'string-suffix? "not a string: ~s" text)]
      [(not (string? suffix))
       (error 'string-suffix? "not a string: ~s" suffix)]
      [else
       (let ([text-len (string-length text)]
	     [suffix-len (string-length suffix)])
	 (and (>= text-len suffix-len)
	      (string=? (substring text
				   (- text-len suffix-len)
				   text-len)
			suffix)))])))

;;;;;;;;;;

(define string->list
  (lambda (s)
    (if (string? s)
	(let ([len (string-length s)])
	  (letrec ([walk (lambda (i)
			   (if (= i len)
			       '()
			       (let ([c (string-ref s i)])
				 (if (char=? c #\\)
				     (cons c
					   (cons c
						 (walk (+ i 1))))
				     (cons c
					   (walk (+ i 1)))))))])
	    (walk 0)))
	(error 'string->list "not a string: ~s" s))))

;;;;;;;;;;

(define string->list-of-chars
  (lambda (s)
    (cons #\" (append (string->list s) (cons #\" '())))))

(define integer->list-of-chars
  (lambda (n)
    (letrec ([loop
	      (lambda (n a)
		(let ([a (cons (string-ref "0123456789" (remainder n 10)) a)])
		  (if (< n 10)
		      a
		      (loop (quotient n 10) a))))])
      (if (negative? n)
	  (cons #\- (loop (- 0 n) '()))
	  (loop n '())))))

(define list-of-values->list-of-chars
  (lambda (vs)
    (letrec ([walk
	      (lambda (a d)
		(append
		  (value->list-of-chars a)
		  (if (null? d)
		      (cons #\)
			    '())
		      (cons char:space
			    (if (pair? d)
				(walk (car d) (cdr d))
				(cons #\.
				      (cons char:space
					    (append
					      (value->list-of-chars d)
					      (cons #\)
						    '())))))))))])
      (cond
	[(null? vs)
	 (list #\( #\))]
	[(pair? vs)
	 (cons #\(
	       (walk (car vs) (cdr vs)))]
	[else
	 (error 'list-of-values->list-of-chars
		"not a list: ~s" vs)]))))

(define vector->list-of-chars
  (lambda (v)
    (let ([len (vector-length v)])
      (cons #\#
	    (append (integer->list-of-chars len)
		    (cons #\(
			  (letrec ([walk
				    (lambda (i)
				      (if (= i len)
					  (cons #\) '())
					  (cons char:space
						(append (value->list-of-chars
							  (vector-ref v i))
							(walk (+ i 1))))))])
			    (if (= len 0)
				(cons #\) '())
				(append (value->list-of-chars (vector-ref v 0))
					(walk 1))))))))))

(define value->list-of-chars
  (lambda (v)
    (cond
      [(list? v)
       (list-of-values->list-of-chars v)]
      [(boolean? v)
       (if v
	   (string->list "#t")
	   (string->list "#f"))]
      [(integer? v)
       (integer->list-of-chars v)]
      [(char? v)
       (cons v '())]
      [(string? v)
       (string->list-of-chars v)]
      [(symbol? v)
       (string->list (symbol->string v))]
      [(procedure? v)
       (string->list "#<procedure>")]
      [(vector? v)
       (vector->list-of-chars v)]
      [(input-port? v)
       (string->list "#<input-port>")]
      [(output-port? v)
       (string->list "#<output-port>")]
      [(eof-object? v)
       (string->list "#<eof-object>")]
      [else
       (error 'value->list-of-chars
	      "unrecognized value: ~s" v)])))

; (define prepend-value->list-of-chars
;   (lambda (v a)
;     ...))

(define value->string
  (lambda (v)
    (apply string (value->list-of-chars v))))

;;;;;;;;;;

;;; end of "string.scm"
