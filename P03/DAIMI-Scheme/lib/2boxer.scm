;;; DAIMI-Scheme/lib/2boxer.scm
;;; a boxer for set!'ed local variables in DAIMI-Scheme desugared programs
;;; Olivier Danvy <danvy@brics.dk>
;;; October-November 2002, October 2003


;;;;;;;;;;
;;; 

(define-record (Boxed-Program the-definitions the-expression))

(define-record (Boxed-Abstraction formals body mutated))
(define-record (Boxed-Variable x))
(define-record (Boxed-LetRecExpr formals lambdas body mutated))
(define-record (Abstraction-val lam))
(define-record (Abstraction-box lam))

;;;;;;;;;;
;;; 

(define mutable-toplevel-variables '())

(define add-a-mutable-toplevel-variable!
  (lambda (x)
    (if (member x mutable-toplevel-variables)
	"What's new?"
	(set! mutable-toplevel-variables
	      (cons x mutable-toplevel-variables)))))


;;;;;;;;;;
;;; 

(define scan-expression
  (lambda (e globals)
    (letrec ([walks
	      (lambda (es locals updates k)
		(if (null? es)
		    (k '() updates)
		    (walk (car es)
			  locals
			  updates
			  (lambda (e updates)
			    (walks (cdr es)
				   locals
				   updates
				   (lambda (es updates)
				     (k (cons e es) updates)))))))]
	     [walk
	      (lambda (e locals updates k)
		(case-record e
		  [(Literal l)
		   (k e updates)]
		  [(Variable x)
		   (k e updates)]
		  [(Abstraction xs b)
		   (walk b
			 (cons (if (symbol? xs)
				   (list xs)
				   xs)
			       locals)
			 (cons '() updates)
			 (lambda (b updates)
			   (k (make-Boxed-Abstraction xs b (car updates))
			      (cdr updates))))]
		  [(IfThenElse test-exp then-exp else-exp)
		   (walk test-exp
			 locals
			 updates
			 (lambda (test-exp updates)
			   (walk then-exp
				 locals
				 updates
				 (lambda (then-exp updates)
				   (walk else-exp
					 locals
					 updates
					 (lambda (else-exp updates)
					   (k (make-IfThenElse test-exp
							       then-exp
							       else-exp)
					      updates)))))))]
		  [(Application e es)
		   (walk e
			 locals
			 updates
			 (lambda (e updates)
			   (walks es
				  locals
				  updates
				  (lambda (es updates)
				    (k (make-Application e es)
				       updates)))))]
		  [(SequenceExpr2 e es)
		   (walk e
			 locals
			 updates
			 (lambda (e updates)
			   (walks es
				  locals
				  updates
				  (lambda (es updates)
				    (k (make-SequenceExpr2 e es)
				       updates)))))]
		  [(LetRecExpr xs ls b)
		   (let ([locals (cons xs locals)]
			 [updates (cons '() updates)])
		     (walks ls
			    locals
			    updates
			    (lambda (ls updates)
			      (walk b
				    locals
				    updates
				    (lambda (b updates)
				      (k (make-Boxed-LetRecExpr xs
								ls
								b
								(car updates))
					 (cdr updates)))))))]
		  [(Assign x e)
		   (walk e
			 locals
			 (insert x locals updates globals)
			 (lambda (e updates)
			   (k (if (ormap1 (lambda (xs)
					    (member x xs))
					  locals)
				  (make-Application
				    (make-Variable 'vector-set!)
				    (list
				      (make-Boxed-Variable x)
				      (make-Literal (make-Integer 0))
				      e))
				  (make-Assign 
				    x
				    e))
			      updates)))]
		  [else
		   (error 'scan-expression "quasar: ~s" e)]))]
	     [insert
	      (lambda (x locals updates globals)
		(letrec ([traverse
			  (lambda (locals updates)
			    (cond
			      [(null? locals)
			       (if (or (member x globals)
				       (member x predefined-procedures))
				   (begin
				     (add-a-mutable-toplevel-variable! x)
				     '())
				   (error 'scan-expression
					  "undeclared variable: ~s" x))]
			      [(member x (car locals))
			       (if (member x (car updates))
				   updates
				   (cons (cons x (car updates))
					 (cdr updates)))]
			      [else
			       (cons (car updates)
				     (traverse (cdr locals)
					       (cdr updates)))]))])
		  (traverse locals updates)))])
      (walk e '() '() (lambda (e updates)
			(if (null? updates)
			    e
			    (error 'scan-expression
				   "non-null updates on return: ~s"
				   updates)))))))


;;;;;;;;;;
;;;

(define box-expression
  (lambda (e globals)
    (letrec ([wrap-let-expression
	      (lambda (mutables e)
		(if (null? mutables)
		    e
		    (make-Application
		      (make-Abstraction
			mutables
			e)
		      (map (lambda (mutable)
			     (make-Application
			       (make-Variable 'vector)
			       (list (make-Variable mutable))))
			   mutables))))]
	     [walk-application
	      (lambda (e es)
		(case-record e
		  [(Abstraction xs b)
		   (if (symbol? xs)
		       (make-Application
			 (make-Abstraction (list xs) b)
			 (letrec ([walk
				   (lambda (es)
				     (if (null? es)
					 (make-Literal (make-Nil))
					 (make-Application
					   (make-Variable 'cons)
					   (list
					     (car es)
					     (walk (cdr es))))))])
			   (list (walk es))))
		       (make-Application e es))]
		  [else
		   (make-Application e es)]))]
	     [walk
	      (lambda (e env-var env-mut)
		(case-record e
		  [(Literal l)
		   e]
		  [(Variable x)
		   (cond
		     [(mutable? x env-var env-mut)
		      (make-Application
			(make-Variable 'vector-ref)
			(list
			  (make-Variable x)
			  (make-Literal (make-Integer 0))))]
		     [(member x mutable-toplevel-variables)
		      (make-Application
			(make-Variable 'dereference-toplevel-variable)
			(list
			  (make-Variable x)))]
		     [else
		      e])]
		  [(Boxed-Variable x)
		   (make-Variable x)]
		  [(Boxed-Abstraction xs b mutables)
		   (make-Abstraction
		     xs
		     (wrap-let-expression
		       mutables
		       (walk b
			     (cons (if (symbol? xs)
				       (list xs)
				       xs)
				   env-var)
			     (cons mutables env-mut))))]
		  [(IfThenElse test-exp then-exp else-exp)
		   (make-IfThenElse (walk test-exp env-var env-mut)
				    (walk then-exp env-var env-mut)
				    (walk else-exp env-var env-mut))]
		  [(Application e es)
		   (walk-application (walk e env-var env-mut)
				     (map1 (lambda (e)
					     (walk e env-var env-mut))
					   es))]
		  [(SequenceExpr2 e es)
		   (make-SequenceExpr2 (walk e env-var env-mut)
				       (map1 (lambda (e)
					       (walk e env-var env-mut))
					     es))]
		  [(Boxed-LetRecExpr xs ls b mutables)
		   (let ([env-var (cons xs env-var)]
			 [env-mut (cons mutables env-mut)])
		     (make-LetRecExpr xs
				      (map (lambda (x l)
					     (if (member x mutables)
						 (make-Abstraction-box
						   (walk l env-var env-mut))
						 (make-Abstraction-val
						   (walk l env-var env-mut))))
					   xs
					   ls)
				      (walk b env-var env-mut)))]
		  [(Assign x e)
		   (make-Assign x (walk e env-var env-mut))]
		  [else
		   (error 'box-expression "quasar: ~s" e)]))]
	     [mutable?
	      (lambda (x env-var env-mut)
		(letrec ([loop
			  (lambda (vars muts)
			    (if (null? muts)
				(if (null? vars)
				    #f
				    (error 'box-expression
					   "mismatch between ~s and ~s"
					   env-var env-mut))
				(if (null? vars)
				    (error 'box-expression
					   "mismatch between ~s and ~s"
					   env-var env-mut)
				    (if (member x (car muts))
					(if (member x (car vars))
					    #t
					    (error 'box-expression
						   "~x in ~s but not in ~s"
						   x env-var env-mut))
					(loop (cdr vars) (cdr muts))))))])
		  (loop env-var env-mut)))])
      (walk (scan-expression e globals) '() '()))))

(define box-definitions
  (lambda (ds globals)
    (letrec ([walk
	      (lambda (ds)
		(if (null? ds)
		    '()
		    (case-record (car ds)
		      [(DefExp name expression)
		       (cons (make-DefExp name
					  (box-expression expression
							  globals))
			     (walk (cdr ds)))]
		      [else
		       (error 'box-definitions
			      "not a definition: ~s" (car ds))])))])
      (walk ds))))


;;;;;;;;;;
;;;

(define box-program
  (lambda (p)
    (case-record p
      [(Desugared-Program the-definitions the-expression)
       (begin
	 (warn "Boxing")
	 (set! mutable-toplevel-variables '())
	 (let* ([globals (map1 (lambda (d)
				 (case-record d
				   [(DefExp name expression)
				    name]
				   [else
				    (error 'box-program
					   "not a definition: ~s" d)]))
			       the-definitions)]
		[ds (box-definitions the-definitions globals)]
		[e (box-expression the-expression globals)])
	   (begin
	     (warnl " / Boxed")
	     (make-Boxed-Program
	       (if (null? mutable-toplevel-variables)
		   ds
		   (cons (make-DefExp 'dereference-toplevel-variable
				      (let ([x (gensym! "the-mutable-guy%")])
					(make-Abstraction
					  (list x)
					  (make-Variable x))))
			 ds))
	       e))))]
      [else
       (error 'box-program "not a desugared program")])))


;;;;;;;;;;

;;; end of "2boxer.scm"
