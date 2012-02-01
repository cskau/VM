;;; dejlisp.scm
;;; an interactive interpreter for a subset of Scheme
;;; exit with ^D

;;;;;;;;;;

(load-relative "/users/courses/dOvs/Project03/DAIMI-Scheme/lib.DAIMI-Scheme/standard-prelude.scm") 

(define _eval
  (lambda (e r g)
    (cond
      [(integer? e)
       e]
      [(symbol? e)
       (lookup e r g)]
      [(pair? e)
       (case (car e)
	 [(quote)
	  (eval-quote (cadr e) r g)]
	 [(lambda)
	  (eval-lambda (cadr e) (caddr e) r)]
	 [(if)
	  (eval-if (cadr e) (caddr e) (cadddr e) r g)]
	 [else
	  (apply ((_eval (car e) r g) g)
		 (map (lambda (e)
			(_eval e r g))
		      (cdr e)))])]
      [else
       (error '_eval "quasar: ~s" e)])))

(define lookup
  (lambda (x r g)
    (letrec ([walk (lambda (r)
		     (cond
		       [(null? r)
			(lookup-global x g)]
		       [(eqv? x (car (car r)))
			(cdr (car r))]
		       [else
			(walk (cdr r))]))])
      (walk r))))

(define lookup-global
  (lambda (x g)
    (letrec ([walk (lambda (g)
		     (cond
		       [(null? g)
			(lookup-predefined x)]
		       [(eqv? x (car (car g)))
			(cdr (car g))]
		       [else
			(walk (cdr g))]))])
      (walk g))))

(define lookup-predefined
  (lambda (x)
    (case x
      [(car) (lambda (global-environment) car)]
      [(cdr) (lambda (global-environment) cdr)]
      [(cons) (lambda (global-environment) cons)]
      [(null?) (lambda (global-environment) null?)]
      [(pair?) (lambda (global-environment) pair?)]
      [(eqv?) (lambda (global-environment) eqv?)]
      [(equal?) (lambda (global-environment) equal?)]
      [(apply) (lambda (global-environment)
		 (lambda (f xs)
		   (apply (f global-environment) xs)))]
      [(=) (lambda (global-environment) =)]
      [(+) (lambda (global-environment) +)]
      [(-) (lambda (global-environment) -)]
      [(*) (lambda (global-environment) *)]
      [else (error 'lookup "Unbound variable: ~s" x)])))

(define eval-quote
  (lambda (e r g)
    e))

(define eval-lambda
  (lambda (formals body local-environment)
    (if (pair? formals)
	(lambda (global-environment)
	  (lambda actuals
	    (letrec ([walk
		      (lambda (formals actuals)
			(if (null? formals)
			    local-environment
			    (cons (cons (car formals)
					(car actuals))
				  (walk (cdr formals)
					(cdr actuals)))))])
	      (_eval body
		     (walk formals actuals)
		     global-environment))))
	(lambda (global-environment)
	  (lambda actuals
	    (_eval body
		   (cons (cons formals actuals) local-environment)
		   global-environment))))))

(define eval-if
  (lambda (test-exp then-exp else-exp environment global-environment)
    (if (_eval test-exp environment global-environment)
	(_eval then-exp environment global-environment)
	(_eval else-exp environment global-environment))))

(define prompt-read
  (lambda (s)
    (begin
      (display s (current-output-port))
      (read))))

(define toplevel
  (lambda ()
    (letrec ([same-procedure-as-last-year
	      (lambda (global-environment)
		(let ([it (prompt-read ">>> ")])
		  (cond
		    [(eof-object? it)
		     (begin
		       (newline (current-output-port))
		       "Nice when it stops")]
                    [(and (pair? it)
			  (eqv? (car it) 'exit))
		     (if (null? (cdr it))
			 "Nice when it stops"
			 (let ([it (cdr it)])
			   (if (and (pair? it)
				    (null? (cdr it)))
			       (car it)
			       it)))]
		    [(and (pair? it)
			  (eqv? (car it) 'define))
		     (same-procedure-as-last-year
		       (cons (cons (car (cdr it))
				   (_eval (car (cdr (cdr it)))
					  '()
					  global-environment))
			     global-environment))]
		    [else
		     (begin
		       (write (_eval it '() global-environment))
		       (newline (current-output-port))
		       (same-procedure-as-last-year global-environment))])))])
      (begin
	(newline (current-output-port))
	(display "Hello world.")
	(newline (current-output-port))
	(same-procedure-as-last-year '())))))

(toplevel)

;;;;;;;;;;
;;; end of dejlisp.scm
