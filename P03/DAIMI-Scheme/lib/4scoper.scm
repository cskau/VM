;;; DAIMI-Scheme/lib/scoper.scm
;;; a scoper for linearized DAIMI-Scheme programs
;;; (leaving Scheme land)
;;; Olivier Danvy <danvy@brics.dk>
;;; October 2002, October 2003


;;;;;;;;;;
;;;

(define-record (Scoped-Program the-definitions the-expression))

(define-record (ScoDefExp x e))

(define-record (ScoLiteral l))
(define-record (ScoVariable x kind offset))
(define-record (ScoVariableTmp int))
(define-record (ScoVariableJoin int))
(define-record (ScoAbstraction-deep xs b))
(define-record (ScoAbstraction-flat xs b freevars))

(define-record (ScoIfThenElse test-exp then-exp else-exp))
(define-record (ScoApplication e es))
(define-record (ScoLetRecExpr xs ls b))
(define-record (ScoAssign int v))
(define-record (ScoAssignLam int l))
(define-record (ScoAssignApp int c))
(define-record (ScoBindCmp int c b))
(define-record (ScoBindVal int v b))
(define-record (ScoReturn v))
(define-record (ScoBindJoin int c b))
(define-record (ScoSeq int c1 c2))

;;;;;;;;;;
;;;

(define compute-freevars
  (lambda (e r globals)
    (letrec ([walk-expression
	      (lambda (e locals a)
		(case-record e
		  [(LinIfThenElse test-exp then-exp else-exp)
		   (walk-value
		     test-exp
		     locals
		     (walk-expression
		       then-exp
		       locals
		       (walk-expression
			 else-exp
			 locals
			 a)))]
		  [(LinApplication v vs)
		   (walk-values vs locals (walk-value v locals a))]
		  [(LinLetRecExpr xs ls b)
		   (let ([locals (cons xs locals)])
		     (walk-expression b
				      locals
				      (walk-values-rec ls
						       locals
						       a)))]
		  [(LinBindCmp i e b)
		   (walk-expression b locals (walk-named e locals a))]
		  [(LinBindVal i v b)
		   (walk-expression b locals (walk-value v locals a))]
		  [(LinSeq i e b)
		   (walk-expression b locals (walk-named e locals a))]
		  [(LinReturn v)
		   (walk-value v locals a)]
		  [(LinBindJoin i l b)
		   (walk-expression b locals (walk-value l locals a))]
		  [(LinAssign x v)
		   (walk-value v locals a)]
		  [(LinAssignLam x l)
		   (walk-value l locals a)]
		  [(LinAssignApp x c)
		   (walk-expression c locals a)]
		  [else
		   (error 'compute-freevars "quasar: ~s" e)]))]
	     [walk-value
	      (lambda (v locals a)
		(case-record v
		  [(LinLiteral l)
		   a]
		  [(LinVariable x)
		   (cond
		     [(ormap1 (lambda (xs)
				(member x xs))
			      locals)
		      a]
		     [(ormap1 (lambda (xs)
				(member x xs))
			      r)
		      (if (memq x a)
			  a
			  (cons x a))]
		     [else
		      a])]
		  [(LinVariableTmp i)
		   a]
		  [(LinVariableJoin i)
		   a]
		  [(LinAbstraction xs b)
		   (walk-expression b
				    (if (symbol? xs)
					(cons (list xs) locals)
					(cons xs locals))
				    a)]
		  [else
		   (error 'compute-freevars "quasar: ~s" e)]))]
	     [walk-values
	      (lambda (vs locals a)
		(if (null? vs)
		    a
		    (walk-values (cdr vs)
				 locals
				 (walk-value (car vs)
					     locals
					     a))))]
	     [walk-values-rec
	      (lambda (vs locals a)
		(if (null? vs)
		    a
		    (walk-values-rec (cdr vs)
				     locals
				     (walk-value (case-record (car vs)
						   [(Abstraction-val lam)
						    lam]
						   [(Abstraction-box lam)
						    lam]
						   [else
						    (error 'walk-values-rec
							   "illegal header: ~s"
							   (car vs))])
						 locals
						 a))))]
	     [walk-named
	      (lambda (c locals a)
		(case-record c
		  [(LinApplication v vs)
		   (walk-values vs locals (walk-value v locals a))]
		  [(LinAssign x v)
		   (walk-value v locals a)]
		  [(LinAbstraction xs b)
		   (walk-expression b
				    (if (symbol? xs)
					(cons (list xs) locals)
					(cons xs locals))
				    a)]
		  [else
		   (error 'walk-named
			  "not a named computation: ~s" c)]))])
      (case-record e
	[(LinAbstraction xs b)
	 (walk-expression b
			  (list (if (symbol? xs)
				    (list xs)
				    xs))
			  '())]
	[else
	 (error 'compute-freevars
		"not a lambda-abstraction: ~s"
		e)]))))

(define extend-for
  (lambda (xs r)
    (if (symbol? xs)
	(cons (list xs) r)
	(cons xs r))))

(define ref-list-kind
  (lambda (xs x)
    (letrec ([loop
	      (lambda (ys offset)
		(cond
		  [(null? ys)
		   -1]
		  [(eqv? (car ys) x)
		   offset]
		  [else
		   (loop (cdr ys) (+ offset 1))]))])
      (loop xs 0))))

(define scope-lookup
  (lambda (x r-orig globals)
    (letrec ([walk
	      (lambda (lex r)
		(if (null? r)
		    (let ([offset (ref-list-kind globals x)])
		      (if (= offset -1)
			  (let ([offset (ref-list-kind predefined-procedures x)])
			    (if (= offset -1)
				(error 'scope-lookup
				       "undeclared variable: ~s in ~s"
				       x r-orig)
				(make-ScoVariable x 'lib offset)))
			  (make-ScoVariable x 'glo offset)))
		    (let ([offset (ref-list-kind (car r) x)])
		      (if (= offset -1)
			  (walk (+ lex 1) (cdr r))
			  (make-ScoVariable x lex offset)))))])
      (walk 0 r-orig))))


;;;;;;;;;;
;;;

(define scope-value
  (lambda (e r globals)
    (case-record e
      [(LinLiteral l)
       (make-ScoLiteral l)]
      [(LinVariable x)
       (scope-lookup x r globals)]
      [(LinVariableTmp i)
       (make-ScoVariableTmp i)]
      [(LinVariableJoin i)
       (make-ScoVariableJoin i)]
      [(LinAbstraction xs b)
       (let ([freevars (compute-freevars e r globals)])
	 (make-ScoAbstraction-flat
	   xs
	   (scope-expression b
			     (list (if (symbol? xs)
				       (list xs)
				       xs)
				   freevars)
			     globals)
	   (map (lambda (freevar)
		  (scope-lookup freevar r '()))
		freevars)))]
      [else
       (error 'scope-value "quasar: ~s" e)])))

(define scope-expression
  (lambda (e r globals)
    (case-record e
      [(LinIfThenElse test-exp then-exp else-exp)
       (make-ScoIfThenElse (scope-value test-exp r globals)
			   (scope-expression then-exp r globals)
			   (scope-expression else-exp r globals))]
      [(LinApplication v vs)
       (scope-application v
			  (map (lambda (v)
				 (scope-value v r globals))
			       vs)
			  r
			  globals)]
      [(LinLetRecExpr xs ls b)
       (let ([ext-r (cons xs r)])
	 (make-ScoLetRecExpr xs
			     (map (lambda (l)
				    (case-record l
				      [(Abstraction-val lam)
				       (make-Abstraction-val
					 (case-record lam
					   [(LinAbstraction xs b)
					    (make-ScoAbstraction-deep
					      xs
					      (scope-expression
						b
						(extend-for xs ext-r)
						globals))]
					   [else
					    (error 'scope-expression
						   "illegal header: ~s"
						   lam)]))]
				      [(Abstraction-box lam)
				       (make-Abstraction-box
					 (case-record lam
					   [(LinAbstraction xs b)
					    (make-ScoAbstraction-deep
					      xs
					      (scope-expression
						b
						(extend-for xs ext-r)
						globals))]
					   [else
					    (error 'scope-expression
						   "illegal header: ~s"
						   lam)]))]
				      [else
				       (error 'scope-expression
					      "quasar: ~s" l)]))
				  ls)
			     (scope-expression b ext-r globals)))]
      [(LinAssign x v)
       (let ([offset (ref-list-kind globals x)])
	 (if (= offset -1)
	     (let ([offset (ref-list-kind predefined-procedures x)])
	       (if (= offset -1)
		   (error 'scope-expression "unrecognized variable: ~s" x)
		   (make-ScoAssign (cons 'lib offset)
				   (scope-value v r globals))))
	     (make-ScoAssign (cons 'glo offset)
			     (scope-value v r globals))))]
      [(LinAssignLam x l)
       (let ([offset (ref-list-kind globals x)])
	 (if (= offset -1)
	     (let ([offset (ref-list-kind predefined-procedures x)])
	       (if (= offset -1)
		   (error 'scope-expression "unrecognized variable: ~s" x)
		   (make-ScoAssignLam (cons 'lib offset)
				      (case-record l
					[(LinAbstraction xs b)
					 (make-ScoAbstraction-deep
					   xs
					   (scope-expression
					     b
					     (extend-for xs r)
					     globals))]
					[else
					 (error 'scope-expression
						"not a lambda: ~s"
						l)]))))
	     (make-ScoAssignLam (cons 'glo offset)
				(case-record l
				  [(LinAbstraction xs b)
				   (make-ScoAbstraction-deep
				     xs
				     (scope-expression
				       b
				       (extend-for xs r)
				       globals))]
				  [else
				   (error 'scope-expression
					  "not a lambda: ~s"
					  l)]))))]
      [(LinAssignApp x c)
       (let ([offset (ref-list-kind globals x)])
	 (if (= offset -1)
	     (let ([offset (ref-list-kind predefined-procedures x)])
	       (if (= offset -1)
		   (error 'scope-expression "unrecognized variable: ~s" x)
		   (make-ScoAssignApp (cons 'lib offset)
				      (scope-expression c r globals))))
	     (make-ScoAssignApp (cons 'glo offset)
				(scope-expression c r globals))))]
      [(LinBindCmp i c b)
       (make-ScoBindCmp i
			(scope-named c r globals)
			(scope-expression b r globals))]
      [(LinBindVal i v b)
       (make-ScoBindVal i
			(scope-value v r globals)
			(scope-expression b r globals))]
      [(LinReturn v)
       (make-ScoReturn (scope-value v r globals))]
      [(LinBindJoin i l b)
       (make-ScoBindJoin i
			 (case-record l
			   [(LinAbstraction xs b)
			    (make-ScoAbstraction-deep
			      xs
			      (scope-expression b (cons xs r) globals))]
			   [else
			    (error 'scope-expression
				   "unexpected LinBindJoin expression: ~s" e)])
			 (scope-expression b r globals))]
      [(LinSeq i c1 c2)
       (make-ScoSeq i
		    (scope-named c1 r globals)
		    (scope-expression c2 r globals))]
      [else
       (error 'scope-expression "quasar: ~s" e)])))

(define scope-application
  (lambda (raw-v cooked-vs r globals)
    (case-record raw-v
      [(LinAbstraction xs b)
       (make-ScoApplication
	 (make-ScoAbstraction-deep
	   xs
	   (scope-expression b (cons xs r) globals))
	 cooked-vs)]
      [else
       (make-ScoApplication
	 (scope-value raw-v r globals)
	 cooked-vs)])))

(define scope-named
  (lambda (c r globals)
    (case-record c
      [(LinApplication v vs)
       (scope-application v
			  (map (lambda (v)
				 (scope-value v r globals))
			       vs)
			  r
			  globals)]
      [(LinAssign x v)
       (make-ScoAssign
	 (let ([offset (ref-list-kind globals x)])
	   (if (= offset -1)
	       (let ([offset (ref-list-kind predefined-procedures x)])
		 (if (= offset -1)
		     (error 'scope-named
			    "undeclared variable: ~s"
			    x)
		     (cons 'lib offset)))
	       (cons 'glo offset)))
	 (scope-value v r globals))]
      [(LinAbstraction xs b)
       (let ([freevars (compute-freevars c r globals)])
	 (make-ScoAbstraction-flat
	   xs
	   (scope-expression b
			     (list (if (symbol? xs)
				       (list xs)
				       xs)
				   freevars)
			     globals)
	   (map (lambda (freevar)
		  (scope-lookup freevar r '()))
		freevars)))]
      [else
       (error 'scope-named
	      "not a named computation: ~s" c)])))


;;;;;;;;;;
;;;

(define scope-program
  (lambda (p)
    (case-record p
      [(Linearized-Program the-definitions the-expression)
       (begin
	 (warn "Scoping")
	 (let ([globals
		(letrec ([walk
			  (lambda (ds globals)
			    (if (null? ds)
				(reverse globals)
				(case-record (car ds)
				  [(LinDefExp x e)
				   (walk (cdr ds)
					 (if (or (member x globals)
						 (member x predefined-procedures))
					     (begin
					       (if (or (eqv? x 'call-with-input-file)
						       (eqv? x 'call-with-output-file))
						   "Sic transit."
						   (warn (format
							   " Multiple definition for ~s"
							   x)))
					       globals)
					     (cons x globals)))]
				  [else
				   (error 'scope-program
					  "not a definition: ~s" (car ds))])))])
		  (walk the-definitions '()))])
	   (let* ([ds (map1 (lambda (d)
			      (case-record d
				[(LinDefExp x e)
				 (make-ScoDefExp
				   x
				   (scope-expression e '() globals))]
				[else
				 (error 'scope-program
					"not a definition: ~s" d)]))
			    the-definitions)]
		  [e (scope-expression the-expression '() globals)])
	     (begin
	       (warnl " / Scoped")
	       (make-Scoped-Program ds e)))))]
      [else
       (error 'scope-program "not a linearized program")])))


;;;;;;;;;;

;;; end of "scoper.scm"
