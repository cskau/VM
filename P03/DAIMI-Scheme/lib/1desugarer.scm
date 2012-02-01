;;; DAIMI-Scheme/lib/1desugarer.scm
;;; a desugarer for DAIMI Scheme
;;; Olivier Danvy <danvy@brics.dk>
;;; October 2002, October 2003


;;;;;;;;;;
;;; 

(define-record (Desugared-Program the-definitions the-expression))

(define-record (SequenceExpr2 e es))


;;;;;;;;;;
;;; 

(define transmogrify-quoted-pair-expression
  (lambda (e)
    (make-Application (make-Variable 'cons)
		      (list (transmogrify-quoted-expression (car e))
			    (transmogrify-quoted-expression (cdr e))))))

(define transmogrify-quoted-non-pair-expression
  (lambda (e)
    (cond
      [(integer? e)
       (make-Literal (make-Integer e))]
      [(boolean? e)
       (make-Literal (make-Boolean e))]
      [(string? e)
       (make-Literal (make-String e))]
      [(char? e)
       (make-Literal (make-Character e))]
      [(symbol? e)
       (make-Literal (make-Symbol e))]
      [(null? e)
       (make-Literal (make-Nil))]
      [else
       (error 'transmogrify-quoted-non-pair-expression "Quasar: ~s" e)])))

(define transmogrify-quoted-expression
  (lambda (e)
    (if (pair? e)
	(transmogrify-quoted-pair-expression e)
	(transmogrify-quoted-non-pair-expression e))))

(define *immutable* '())

(define file-immutable!
  (lambda (name expression)
    (set! *immutable* (cons (make-DefExp name
					 expression)
			    *immutable*))))


;;;;;;;;;;
;;; 

(define simple-expression?
  (lambda (e)
    (case-record e
      [(Literal l)
       #t]
      [(Variable x)
       #t]
      [(Abstraction xs b)
       #t]
      [(IfThenElse test-exp then-exp else-exp)
       #f]
      [(Application e es)
       #f]
      [(CondExpr ts es)
       #f]
      [(CaseExpr e ls es)
       #f]
      [(CaseRecordExpr e ls es)
       #f]
      [(ConjExpr es)
       #f]
      [(DisjExpr es)
       #f]
      [(LetExpr xs es b)
       #f]
      [(SLetExpr xs es b)
       #f]
      [(LetRecExpr xs ls b)
       #f]
      [(SequenceExpr es)
       #f]
      [(Suspend e)
       #f]
      [(Delay e)
       #f]
      [(Assign x e)
       #f]
      [(Quotation e)
       #t]
      [else
       (error 'simple-expression? "quasar: ~s" e)])))


;;;;;;;;;;
;;; 

(define extend-substitution
  (lambda (xs vs env)
    (variadic-left-fold (lambda (env var val)
			  (cons (cons var val) env))
			env
			xs
			vs)))

;;;;;;;;;;

(define desugar-expression
  (lambda (e)
    (letrec ([substitute
	      (lambda (e env)
		(case-record e
		  [(Literal l)
		   e]
		  [(Variable x)
		   (let ([a (assq x env)])
		     (if a
			 (cdr a)
			 e))]
		  [(Abstraction xs b)
		   (let ([ys (if (symbol? xs)
				 (gensym! (string-append
					    (symbol->string xs)
					    "%"))
				 (map (lambda (x)
					(gensym! (string-append
						   (symbol->string x)
						   "%")))
				      xs))])
		     (make-Abstraction
		       ys
		       (substitute
			 b
			 (if (symbol? xs)
			     (cons (cons xs (make-Variable ys)) env)
			     (extend-substitution xs
						  (map make-Variable ys)
						  env)))))]
		  [(IfThenElse test-exp then-exp else-exp)
		   (make-IfThenElse-if-you-really-have-to
		     (substitute test-exp env)
		     (lambda ()
		       (substitute then-exp env))
		     (lambda ()
		       (substitute else-exp env)))]
		  [(Application e es)
		   (make-Application-and-check-arity-if-possible
		     (substitute e env)
		     (map (lambda (e)
			    (substitute e env))
			  es))]
		  [(CondExpr ts es)
		   (letrec ([loop
			     (lambda (ts es)
			       (if (null? (cdr ts))
				   (substitute (car es) env)
				   (make-IfThenElse-if-you-really-have-to
				     (substitute (car ts) env)
				     (lambda () (substitute (car es) env))
				     (lambda () (loop (cdr ts) (cdr es))))))])
		     (loop ts es))]
		  [(CaseExpr e ls es)
		   (let* ([d (substitute e env)]
			  [f (lambda (d)
			       (letrec ([loop
					 (lambda (ls es)
					   (cond
					     [(null? (cdr ls))
					      (substitute (car es) env)]
					     [(null? (car ls))
					      (loop (cdr ls) (cdr es))]
					     [else
					      (make-IfThenElse
						(letrec ([traverse
							  (lambda (ls)
							    (if (null? (cdr ls))
								(make-Application
								  (make-Variable 'eqv?)
								  (list
								    d
								    (transmogrify-quoted-expression
								      (car ls))))
								(make-IfThenElse
								  (make-Application
								    (make-Variable 'eqv?)
								    (list
								      d
								      (transmogrify-quoted-expression
									(car ls))))
								  (make-Literal
								    (make-Boolean #t))
								  (traverse (cdr ls)))))])
						  (traverse (car ls)))
						(substitute (car es) env)
						(loop (cdr ls) (cdr es)))]))])
				 (loop ls es)))])
		     (if (simple-expression? d)
			 (f d)
			 (let ([new (gensym! "case%")])
			   (make-Application
			     (make-Abstraction
			       (list new)
			       (f (make-Variable new)))
			     (list d)))))]
		  [(CaseRecordExpr e ls es)
		   (let* ([d (substitute e env)]
			  [f (lambda (d)
			       (letrec ([loop
					 (lambda (ls es)
					   (cond
					     [(null? (cdr ls))
					      (substitute (car es) env)]
					     [else
					      (let ([name (car (car ls))]
						    [fields (cdr (car ls))])
						(let ([new-fields (map (lambda (field)
									 (gensym! (string-append
										    (symbol->string
										      field)
										    "%")))
								       fields)])
						  (make-IfThenElse
						    (make-Application
						      (make-Variable
							(string->symbol
							  (string-append
							    "is-"
							    (string-append
							      (symbol->string
								name)
							      "?"))))
						      (list d))
						    (make-Application
						      (make-Abstraction
							new-fields
							(substitute (car es)
								    (extend-substitution
								      fields
								      (map make-Variable
									   new-fields)
								      env)))
						      (letrec ([process
								(lambda (fields offset)
								  (if (null? fields)
								      '()
								      (cons
									(make-Application
									  (make-Variable
									    'vector-ref)
									  (list
									    d
									    (make-Literal
									      (make-Integer
										offset))))
									(process
									  (cdr fields)
									  (+ offset 1)))))])
						      (process fields 1)))
						    (loop (cdr ls) (cdr es)))))]))])
				 (loop ls es)))])
		     (if (simple-expression? d)
			 (f d)
			 (let ([new (gensym! "case-record%")])
			   (make-Application
			     (make-Abstraction
			       (list new)
			       (f (make-Variable new)))
			     (list d)))))]
		  [(ConjExpr es)
		   (letrec ([loop (lambda (es)
				    (if (null? (cdr es))
					(substitute (car es) env)
					(make-IfThenElse-if-you-really-have-to
					  (substitute (car es) env)
					  (lambda () (loop (cdr es)))
					  (lambda () (make-Literal (make-Boolean #f))))))])
		     (if (null? es)
			 (make-Literal (make-Boolean #t))
			 (loop es)))]
		  [(DisjExpr es)
		   (letrec ([loop (lambda (es)
				    (let ([d (substitute (car es) env)])
				      (if (null? (cdr es))
					  d
					  (if (simple-expression? d)
					      (make-IfThenElse-if-you-really-have-to
						d
						(lambda () d)
						(lambda () (loop (cdr es))))
					      (let ([new (gensym! "or%")])
						(make-Application
						  (make-Abstraction
						    (list new)
						    (make-IfThenElse
						      (make-Variable new)
						      (make-Variable new)
						      (loop (cdr es))))
						  (list d)))))))])
		     (if (null? es)
			 (make-Literal (make-Boolean #f))
			 (loop es)))]
		  [(LetExpr xs es b)
		   (let ([ys (map (lambda (x)
				    (gensym! (string-append
					       (symbol->string x)
					       "%")))
				  xs)])
		     (make-Application
		       (make-Abstraction
			 ys
			 (substitute
			   b
			   (extend-substitution
			     xs
			     (map make-Variable ys)
			     env)))
		       (map (lambda (e)
			      (substitute e env))
			    es)))]
		  [(SLetExpr xs es b)
		   (letrec ([loop
			     (lambda (xs es env)
			       (if (null? xs)
				   (substitute b env)
				   (make-Application
				     (make-Abstraction (list (car xs))
						       (loop (cdr xs)
							     (cdr es)
							     (cons (cons (car xs)
									 (make-Variable
									   (car xs)))
								   env)))
				     (list (substitute (car es) env)))))])
		     (loop xs es env))]
		  [(LetRecExpr xs ls b)
		   (let ([ys (map (lambda (x)
				    (gensym! (string-append (symbol->string x) "%")))
				  xs)])
		     (let ([env (extend-substitution xs (map make-Variable ys) env)])
		       (make-LetRecExpr
			 ys
			 (map (lambda (l)
				(substitute l env))
			      ls)
			 (substitute b env))))]
		  [(SequenceExpr es)
		   (letrec ([flatten
			     (lambda (es a)
			       (if (null? es)
				   a
				   (case-record (car es)
				     [(SequenceExpr as)
				      (flatten as
					       (flatten (cdr es) a))]
				     [else
				      (cons (car es)
					    (flatten (cdr es) a))])))]
			    [transparent?
			     (lambda (e)
			       (case-record e
				 [(Literal l)
				  #t]
				 [(Variable x)
				  #t]
				 [(Abstraction xs b)
				  #t]
				 [(IfThenElse test-exp then-exp else-exp)
				  (and (transparent? test-exp)
				       (transparent? then-exp)
				       (transparent? else-exp))]
				 [(Application e es)
				  (and (andmap1 transparent? es)
				       (case-record e
					 [(Abstraction xs b)
					  (transparent? b)]
					 [else
					  #f]))]
				 [(CondExpr ts es)
				  (and (andmap1 transparent? ts)
				       (andmap1 transparent? es))]
				 [(CaseExpr e ls es)
				  (and (transparent? e)
				       (andmap1 transparent? es))]
				 [(CaseRecordExpr e ls es)
				  (and (transparent? e)
				       (andmap1 transparent? es))]
				 [(ConjExpr es)
				  (andmap1 transparent? es)]
				 [(DisjExpr es)
				  (andmap1 transparent? es)]
				 [(LetExpr xs es b)
				  (and (andmap1 transparent? es)
				       (transparent? b))]
				 [(SLetExpr xs es b)
				  (and (andmap1 transparent? es)
				       (transparent? b))]
				 [(LetRecExpr xs ls b)
				  (transparent? b)]
				 [(SequenceExpr es)
				  (andmap1 transparent? es)]
				 [(Suspend e)
				  #t]
				 [(Delay e)
				  #t]
				 [(Assign x e)
				  #f]
				 [(Quotation e)
				  #t]
				 [else
				  (error 'transparent? "quasar: ~s" e)]))]
			    [clean-up
			     (lambda (es)
			       (cond
				 [(null? (cdr es))
				  es]
				 [(transparent? (car es))
				  (clean-up (cdr es))]
				 [else
				  (cons (car es) (clean-up (cdr es)))]))]
			    [process-1
		             ;;; version 1: creates embedded beta-redices
			     (lambda (es)
			       (cond
				 [(null? (cdr es))
				  (substitute (car es) env)]
				 [(simple-expression? (car es) env)
				  (process-1 (cdr es))]
				 [else
				  (make-Application
				    (make-Abstraction
				      (list (gensym! "begin%"))
				      (process-1 (cdr es)))
				    (list (substitute (car es) env)))]))]
			    [process-2
		             ;;; version 2: creates one beta-redex,
		             ;;; relying on left-to-right evaluation of arguments.
			     (lambda (es)
			       (let ([formals (map (lambda (e)
						     (gensym! "begin%"))
						   es)])
				 (make-Application
				   (make-Abstraction
				     formals
				     (make-Variable (car (last-pair formals))))
				   (map (lambda (e)
					  (substitute e env))
					es))))]
			    [process-3
		             ;;; version 3: keeps a Sequence expression.
			     (lambda (es)
			       (make-SequenceExpr (map (lambda (e)
							 (substitute e env))
						       es)))]
			    [process-4
		             ;;; version 4: keeps a Sequence2 expression.
			     (lambda (es)
			       (let ([res (map (lambda (e)
						 (substitute e env))
					       es)])
			       (make-SequenceExpr2 (car res) (cdr res))))])
	             ;;; (process-1 (flatten es '()))
		     ;;; (process-2 (clean-up (flatten es '())))
	             ;;; (process-3 (clean-up (flatten es '())))
	             (process-4 (clean-up (flatten es '())))
		     )]
		  [(Suspend e)
		   (make-Abstraction '() (substitute e env))]
		  [(Delay e)
		   (let ([flag/result (gensym! "delay%")]
			 [tmp (gensym! "delay-tmp%")])
		     (make-Application
		       (make-Abstraction
			 (list flag/result)
			 (make-Abstraction
			   '()
			   (make-IfThenElse
			     (make-Application
			       (make-Variable 'vector-ref)
			       (list
				 (make-Variable flag/result)
				 (make-Literal (make-Integer 0))))
			     (make-Application
			       (make-Variable 'vector-ref)
			       (list
				 (make-Variable flag/result)
				 (make-Literal (make-Integer 1))))
			     (make-Application
			       (make-Abstraction
				 (list tmp)
				 (make-SequenceExpr2
				   (make-Application
				     (make-Variable 'vector-set!)
				     (list
				       (make-Variable flag/result)
				       (make-Literal (make-Integer 1))
				       (make-Variable tmp)))
				   (list
				     (make-Application
				       (make-Variable 'vector-set!)
				       (list
					 (make-Variable flag/result)
					 (make-Literal (make-Integer 0))
					 (make-Literal (make-Boolean #t))))
				     (make-Variable tmp))))
			       (list (substitute e env))))))
		       (list (make-Application
				       (make-Variable 'vector)
				       (list
					 (make-Literal (make-Boolean #f))
					 (make-Literal (make-String "not computed yet")))))))]
		  [(Assign x e)
		   (make-Assign (let ([a (assq x env)])
				  (if a
				      (vector-ref (cdr a) 1)
				      x))
				(substitute e env))]
		  [(Quotation e)
		   (if (pair? e)
		       (let ([q (gensym! "immutable%")])
			 (begin
			   (file-immutable! q (transmogrify-quoted-pair-expression e))
			   (make-Variable q)))
		       (transmogrify-quoted-non-pair-expression e))]
		  [else
		   (error 'desugar-expression "quasar: ~s" e)]))])
      (substitute e '()))))

(define make-IfThenElse-if-you-really-have-to
  (lambda (test-exp thunk-then-exp thunk-else-exp)
    (case-record test-exp
      [(Literal l)
       (case-record l
	 [(Boolean b)
	  (if b
	      (thunk-then-exp)
	      (thunk-else-exp))]
	 [else
	  (thunk-then-exp)])]
      [else
       (make-IfThenElse test-exp
			(thunk-then-exp)
			(thunk-else-exp))])))

(define make-Application-and-check-arity-if-possible
  (lambda (e es)
    (case-record e
      [(Abstraction xs b)
       (if (or (symbol? xs)
	       (= (length xs) (length es)))
	   (make-Application e es)
	   (error 'desugar-expression
		  "arity mismatch in beta-redex: ~s and ~s" xs es))]
      [else
       (make-Application e es)])))

;;;;;;;;;;

(define desugar-definition
  (lambda (d)
    (case-record d
      [(DefRec name fields)
       (let ([sname (symbol->string name)]
	     [len (+ (length fields) 1)])
	 (let ([make-name
		(string->symbol (string-append "make-" sname))]
	       [name?
		(string->symbol (string-append "is-" (string-append sname "?")))]
	       [indices
		(letrec ([loop
			  (lambda (i)
			    (if (= i len)
				'()
				(cons i (loop (add1 i)))))])
		  (loop 1))]
	       [v (gensym! "record%")])
	   (list
	     (make-DefExp
	       make-name
	       (let ([fresh-fields
		      (map (lambda (x)
			     (string->symbol
				 (string-append (symbol->string x)
						"_")))
			   fields)])
		 (make-Abstraction
		   fresh-fields
		   (make-Application
		     (make-Variable 'vector)
		     (cons (make-Literal (make-Symbol name))
			   (map make-Variable fresh-fields))))))
	     (make-DefExp
	       name?
	       (make-Abstraction
		 (list v)
		 (make-IfThenElse
		   (make-Application
		     (make-Variable 'vector?)
		     (list (make-Variable v)))
		   (make-IfThenElse
		     (make-Application
		       (make-Variable '=)
		       (list (make-Application
			       (make-Variable 'vector-length)
			       (list (make-Variable v)))
			     (make-Literal (make-Integer len))))
		     (make-Application
		       (make-Variable 'eqv?)
		       (list (make-Application
			       (make-Variable 'vector-ref)
			       (list (make-Variable v)
				     (make-Literal (make-Integer 0))))
			     (make-Literal (make-Symbol name))))
		     (make-Literal (make-Boolean #f)))
		   (make-Literal (make-Boolean #f))))))))]
      [(DefExp name expression)
       (list (make-DefExp name (desugar-expression expression)))]
      [else
       (error 'desugar-program "not a definition: ~s" d)])))


;;;;;;;;;;
;;; 

(define check-for-multiple-definitions!
  (lambda (ds)
    (letrec ([loop
	      (lambda (xs)
		(cond
		  [(null? xs)
		   '()]
		  [(memq (car xs) (cdr xs))
		   (cons (car xs) (loop (cdr xs)))]
		  [else
		   (loop (cdr xs))]))])
      (let ([duplicates (loop (map (lambda (d)
				     (case-record d
				       [(DefExp name expression)
					name]
				       [else
					(error 'check-for-multiple-definitions!
					       "not a definition: ~s"
					       d)]))
				   ds))])
	(if (null? duplicates)
	    'ok
	    (error 'check-for-multiple-definitions!
		   "multiple definitions: ~s"
		   duplicates))))))


;;;;;;;;;;
;;;

(define desugar-program
  (lambda (p)
    (case-record p
      [(Parsed-Program the-definitions the-expression)
       (begin
	 (warn "Desugaring")
	 (set! *immutable* '())
	 (let* ([ds (map-append desugar-definition the-definitions)]
		[e (desugar-expression the-expression)])
	   (begin
	     ;(check-for-multiple-definitions! ds)
	     (warnl " / Desugared")
	     (make-Desugared-Program (append (reverse *immutable*) ds) e))))]
      [else
       (error 'desugar-program "not a parsed program")])))

;;;;;;;;;;

;;; end of "1desugarer.scm"
