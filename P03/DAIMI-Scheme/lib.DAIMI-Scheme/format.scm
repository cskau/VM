;;; lib.DAIMI-Scheme/format.scm
;;; DAIMI-Scheme string formatter, a la sprintf in C
;;; For example, evaluating
;;;     (format "foo ~s bar ~s baz \~" 1 '(a b))
;;; yields
;;;     "foo 1 bar (a b) baz ~"

;;; requires "error.scm"
;;; requires "strings.scm"

;;;;;;;;;;

(define format
  (lambda args
    (if (null? args)
	""
	(let ([directive (car args)]
	      [rest (cdr args)])
	  (if (string? directive)
	      (let ([end (string-length directive)])
		(letrec ([walk
			  (lambda (index rest)
			    (letrec ([walk-char
				      (lambda (index)
					(if (= index end)
					    '()
					    (let ([c (string-ref directive
								 index)])
					      (case c
						[(#\~)
						 (walk-control (+ index 1))]
						[(#\\)
						 (walk-quote (+ index 1))]
						[else
						 (cons c (walk-char
							   (+ index 1)))]))))]
				     [walk-control
				      (lambda (index)
					(if (= index end)
					    (error 'format
						   "incomplete directive: ~s"
						   directive)
					    (let ([c (string-ref directive
								 index)])
					      (case c
						[(#\s)
						 (if (null? rest)
						     (error 'format
							    "too few arguments for ~s"
							    directive)
						     (append
						       (value->list-of-chars
							 (car rest))
						       (walk (+ index 1)
							     (cdr rest))))]
						[else
						 (error 'format
							"illegal control character: ~s"
							c)]))))]
				     [walk-quote
				      (lambda (index)
					(if (= index end)
					    (error 'format
						   "incomplete directive: ~s"
						   directive)
					    (cons (string-ref directive index)
						  (walk-char (+ index 1)))))])
			      (if (null? rest)
				  (complete index)
				  (walk-char index))))]
			 [complete
			  (lambda (index)
			    (if (= index end)
				'()
				(let ([c (string-ref directive index)])
				  (case c
				    [(#\~)
				     (error 'format
					    "too many arguments for ~s"
					    directive)]
				    [(#\\)
				     (complete-quote (+ index 1))]
				    [else
				     (cons c (complete (+ index 1)))]))))]
			 [complete-quote
			  (lambda (index)
			    (if (= index end)
				(error 'format
				       "incomplete directive: ~s"
				       directive)
				(cons (string-ref directive index)
				      (complete (+ index 1)))))])
		  (apply string (walk 0 rest))))
	      (error 'format "not a string: ~s" directive))))))

;;;;;;;;;;

;;; end of "format.scm"
