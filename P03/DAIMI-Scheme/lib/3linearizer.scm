;;; DAIMI-Scheme/lib/3linearizer.scm
;;; a linearizer for DAIMI-Scheme boxed programs
;;; Olivier Danvy <danvy@brics.dk>
;;; October-November 2002, October 2003


;;;;;;;;;;
;;; 

(define-record (Linearized-Program the-definitions the-expression))

(define-record (LinDefExp x e))

(define-record (LinLiteral l))
(define-record (LinVariable x))
(define-record (LinVariableTmp int))
(define-record (LinVariableJoin int))
(define-record (LinAbstraction xs b))

(define-record (LinIfThenElse test-exp then-exp else-exp))
(define-record (LinApplication e es))
; (define-record (LinLetExpr xs es b))
(define-record (LinLetRecExpr xs ls b))
(define-record (LinAssign x e))
(define-record (LinAssignLam x l))
(define-record (LinAssignApp x c))
(define-record (LinBindCmp int c b))
(define-record (LinBindVal int v b))
(define-record (LinReturn v))
(define-record (LinBindJoin int c b))
(define-record (LinSeq int c1 c2))

;;;;;;;;;;
;;; 

(define linearize-expression0
  (lambda (e)
    (case-record e
      [(Literal l)
       (make-LinReturn (make-LinLiteral l))]
      [(Variable x)
       (make-LinReturn (make-LinVariable x))]
      [(Abstraction xs b)
       (make-LinReturn (make-LinAbstraction xs
					    (linearize-expression0 b)))]
      [(IfThenElse test-exp then-exp else-exp)
       (linearize-expression22 test-exp
			       0
			       (lambda ()
				 (linearize-expression0 then-exp))
			       (lambda ()
				 (linearize-expression0 else-exp)))]
      [(Application e es)
       (case-record e
	 [(Abstraction xs b)
	  (linearize-expression2s
	    es
	    0
	    (lambda (vs level)
	      (make-LinApplication
		(make-LinAbstraction
		  xs
		  (linearize-expression0 b))
		vs)))]
	 [else
	  (linearize-expression2
	    e
	    0
	    (lambda (v level)
	      (linearize-expression2s
		es
		level
		(lambda (vs level)
		  (make-LinApplication v vs)))))])]
      [(SequenceExpr2 e es)
       (linearize-expression0-seq e es)]
      [(LetRecExpr xs ls b)
       (make-LinLetRecExpr
	 xs
	 (map1 (lambda (l)
		 (case-record l
		   [(Abstraction-val lam)
		    (make-Abstraction-val
		      (case-record lam
			[(Abstraction xs b)
			 (make-LinAbstraction xs
					      (linearize-expression0 b))]
			[else
			 (error 'linearize-expression0
				"invalid letrec header: ~s" l)]))]
		   [(Abstraction-box lam)
		    (make-Abstraction-box
		      (case-record lam
			[(Abstraction xs b)
			 (make-LinAbstraction xs
					      (linearize-expression0 b))]
			[else
			 (error 'linearize-expression0
				"invalid letrec header: ~s" l)]))]
		   [else
		    (error 'linearize-expression0
			   "invalid letrec header: ~s" l)]))
	       ls)
	 (linearize-expression0 b))]
      [(Assign x e)
       (linearize-expression2
	 e
	 0
	 (lambda (v level)
	   (make-LinBindCmp
	     0
	     (make-LinAssign x v)
	     (make-LinReturn (make-LinVariableTmp 0)))))]
      [else
       (error 'linearize-expression0 "quasar: ~s" e)])))

(define linearize-expression0-seq
  (lambda (e es)
    (if (null? es)
	(linearize-expression0 e)
	(linearize-expression3
	  e
	  0
	  (lambda ()
	    (linearize-expression0-seq (car es) (cdr es)))))))

;;;;;;;;;;

(define linearize-expression1
  (lambda (e level k)
    (case-record e
      [(Literal l)
       (make-LinApplication
	 (make-LinVariableJoin k)
	 (list (make-LinLiteral l)))]
      [(Variable x)
       (make-LinApplication
	 (make-LinVariableJoin k)
	 (list (make-LinVariable x)))]
      [(Abstraction xs b)
       (make-LinBindCmp
	 level
	 (make-LinAbstraction xs (linearize-expression0 b))
	 (make-LinApplication
	   (make-LinVariableJoin k)
	   (list (make-LinVariableTmp level))))]
      [(IfThenElse test-exp then-exp else-exp)
       (linearize-expression22 test-exp
			       level
			       (lambda ()
				 (linearize-expression1 then-exp level k))
			       (lambda ()
				 (linearize-expression1 else-exp level k)))]
      [(Application e es)
       (case-record e
	 [(Abstraction xs b)
	  (linearize-expression2s
	    es
	    level
	    (lambda (vs level_vs)
	      (make-LinApplication
		(make-LinAbstraction
		  xs
		  (linearize-expression1 b level k))
		vs)))]
	 [else
	  (linearize-expression2
	    e
	    level
	    (lambda (v level_e)
	      (linearize-expression2s
		es
		level_e
		(lambda (vs level_es)
		  (make-LinBindCmp
		    level
		    (make-LinApplication v vs)
		    (make-LinApplication
		      (make-LinVariableJoin k)
		      (list
			(make-LinVariableTmp level))))))))])]
      [(SequenceExpr2 e es)
       (linearize-expression1-seq e es level k)]
      [(LetRecExpr xs ls b)
       (make-LinLetRecExpr
	 xs
	 (map1 (lambda (l)
		 (case-record l
		   [(Abstraction-val lam)
		    (make-Abstraction-val
		      (case-record lam
			[(Abstraction xs b)
			 (make-LinAbstraction xs
					      (linearize-expression0 b))]
			[else
			 (error 'linearize-expression1
				"invalid letrec header: ~s" l)]))]
		   [(Abstraction-box lam)
		    (make-Abstraction-box
		      (case-record lam
			[(Abstraction xs b)
			 (make-LinAbstraction xs
					      (linearize-expression0 b))]
			[else
			 (error 'linearize-expression1
				"invalid letrec header: ~s" l)]))]
		   [else
		    (error 'linearize-expression1
			   "invalid letrec header: ~s" l)]))
	       ls)
	 (linearize-expression1 b level k))]
      [(Assign x e)
       (linearize-expression2
	 e
	 level
	 (lambda (v new-level)
	   (make-LinBindCmp
	     level
	     (make-LinAssign x v)
	     (make-LinApplication
	       (make-LinVariableJoin k)
	       (list
		 (make-LinVariableTmp level))))))]
      [else
       (error 'linearize-expression1 "quasar: ~s" e)])))

(define linearize-expression1-seq
  (lambda (e es level k)
    (letrec ([traverse
	      (lambda (e es)
		(if (null? es)
		    (linearize-expression1 e level k)
		    (linearize-expression3 e
					   level
					   (lambda ()
					     (traverse (car es)
						       (cdr es))))))])
      (traverse e es))))

;;;;;;;;;;

(define linearize-expression2
  (lambda (e level kappa)
    (case-record e
      [(Literal l)
       (kappa (make-LinLiteral l) level)]
      [(Variable x)
       (kappa (make-LinVariable x) level)]
      [(Abstraction xs b)
       (make-LinBindVal
	 level
	 (make-LinAbstraction xs (linearize-expression0 b))
	 (kappa (make-LinVariableTmp level)
		(+ level 1)))]
      [(IfThenElse test-exp then-exp else-exp)
       (let ([x (gensym! "join%")])
	 (make-LinBindJoin
	   level
	   (make-LinAbstraction
	     (list x)
	     (kappa (make-LinVariable x) level))
	   (let ([new-level (+ level 1)])
	     (linearize-expression22
	       test-exp
	       new-level
	       (lambda ()
		 (linearize-expression1
		   then-exp
		   new-level
		   level))
	       (lambda ()
		 (linearize-expression1
		   else-exp
		   new-level
		   level))))))]
      [(Application e es)
       (case-record e
	 [(Abstraction xs b)
	  (linearize-expression2s
	    es
	    level
	    (lambda (vs level_vs)
	      (make-LinApplication
		(make-LinAbstraction
		  xs
		  (linearize-expression2 b level kappa))
		vs)))]
	 [else
	  (linearize-expression2
	    e
	    level
	    (lambda (v level_e)
	      (linearize-expression2s
		es
		level_e
		(lambda (vs level_es)
		  (make-LinBindCmp
		    level
		    (make-LinApplication v vs)
		    (kappa (make-LinVariableTmp level)
			   (+ level 1)))))))])]
      [(SequenceExpr2 e es)
       (linearize-expression2seq e es level kappa)]
      [(LetRecExpr xs ls b)
       (make-LinLetRecExpr
	 xs
	 (map1 (lambda (l)
		 (case-record l
		   [(Abstraction-val lam)
		    (make-Abstraction-val
		      (case-record lam
			[(Abstraction xs b)
			 (make-LinAbstraction xs
					      (linearize-expression0 b))]
			[else
			 (error 'linearize-expression1
				"invalid letrec header: ~s" l)]))]
		   [(Abstraction-box lam)
		    (make-Abstraction-box
		      (case-record lam
			[(Abstraction xs b)
			 (make-LinAbstraction xs
					      (linearize-expression0 b))]
			[else
			 (error 'linearize-expression1
				"invalid letrec header: ~s" l)]))]
		   [else
		    (error 'linearize-expression2
			   "invalid letrec header: ~s" l)]))
	       ls)
	 (linearize-expression2 b level kappa))]
      [(Assign x e)
       (linearize-expression2 e
			      level
			      (lambda (v new-level)
				(make-LinBindCmp
				  level
				  (make-LinAssign x v)
				  (kappa (make-LinVariableTmp level)
					 level))))]
      [else
       (error 'linearize-expression2 "quasar: ~s" e)])))

(define linearize-expression2s
  (lambda (es level kappa)
    (if (null? es)
	(kappa '() level)
	(linearize-expression2
	  (car es)
	  level
	  (lambda (v new-level)
	    (linearize-expression2s
	      (cdr es)
	      new-level
	      (lambda (vs resulting-level)
		(kappa (cons v vs)
		       resulting-level))))))))

(define linearize-expression2seq
  (lambda (e es level kappa)
    (letrec ([traverse
	      (lambda (e es)
		(if (null? es)
		    (linearize-expression2 e level kappa)
		    (linearize-expression3 e
					   level
					   (lambda ()
					     (traverse (car es)
						       (cdr es))))))])
      (traverse e es))))

;;;;;;;;;;

(define linearize-expression22
  (lambda (e level thunk-if-true thunk-if-false)
    (case-record e
      [(Literal l)
       (case-record l
	 [(Boolean b)
	  (if b
	      (thunk-if-true)
	      (thunk-if-false))]
	 [else
	  (thunk-if-true)])]
      [(Variable x)
       (make-LinIfThenElse
	 (make-LinVariable x)
	 (thunk-if-true)
	 (thunk-if-false))]
      [(Abstraction xs b)
       (thunk-if-true)]
      [(IfThenElse test-exp then-exp else-exp)
       (let ([x (gensym! "join%")])
	 (make-LinBindJoin
	   level
	   (make-LinAbstraction
	     (list x)
	     (make-LinIfThenElse
	       (make-LinVariable x)
	       (thunk-if-true)
	       (thunk-if-false)))
	   (let ([new-level (+ level 1)])
	     (linearize-expression22
	       test-exp
	       new-level
	       (lambda ()
		 (linearize-expression1
		   then-exp
		   new-level
		   level))
	       (lambda ()
		 (linearize-expression1
		 else-exp
		 new-level
		 level))))))]
      [(Application e es)
       (case-record e
	 [(Abstraction xs b)
	  (linearize-expression2s
	    es
	    level
	    (lambda (vs level_vs)
	      (make-LinApplication
		(make-LinAbstraction
		  xs
		  (linearize-expression22
		    b
		    level
		    thunk-if-true
		    thunk-if-false))
		vs)))]
	 [else
	  (linearize-expression2
	    e
	    level
	    (lambda (v level_e)
	      (linearize-expression2s
		es
		level_e
		(lambda (vs level_es)
		  (make-LinBindCmp
		    level
		    (make-LinApplication v vs)
		    (make-LinIfThenElse
		      (make-LinVariableTmp level)
		      (thunk-if-true)
		      (thunk-if-false)))))))])]
      [(SequenceExpr2 e es)
       (linearize-expression22-seq e es level thunk-if-true thunk-if-false)]
      [(LetRecExpr xs ls b)
       (make-LinLetRecExpr
	 xs
	 (map1 (lambda (l)
		 (case-record l
		   [(Abstraction-val lam)
		    (make-Abstraction-val
		      (case-record lam
			[(Abstraction xs b)
			 (make-LinAbstraction xs
					      (linearize-expression0 b))]
			[else
			 (error 'linearize-expression1
				"invalid letrec header: ~s" l)]))]
		   [(Abstraction-box lam)
		    (make-Abstraction-box
		      (case-record lam
			[(Abstraction xs b)
			 (make-LinAbstraction xs
					      (linearize-expression0 b))]
			[else
			 (error 'linearize-expression1
				"invalid letrec header: ~s" l)]))]
		   [else
		    (error 'linearize-expression22
			   "invalid letrec header: ~s" l)]))
	       ls)
	 (linearize-expression22 b level thunk-if-true thunk-if-false))]
      [(Assign x e)
       (linearize-expression2
	 e
	 level
	 (lambda (v new-level)
	   (make-LinBindCmp
	     level
	     (make-LinAssign x v)
	     (thunk-if-true))))]
      [else
       (error 'linearize-expression22 "quasar: ~s" e)])))

(define linearize-expression22-seq
  (lambda (e es level thunk-if-true thunk-if-false)
    (letrec ([traverse
	      (lambda (e es)
		(if (null? es)
		    (linearize-expression22 e
					    level
					    thunk-if-true
					    thunk-if-false)
		    (linearize-expression3 e
					   level
					   (lambda ()
					     (traverse (car es)
						       (cdr es))))))])
      (traverse e es))))


(define linearize-expression3
  (lambda (e level kappa)
    (case-record e
      [(Literal l)
       (kappa)]
      [(Variable x)
       (kappa)]
      [(Abstraction xs b)
       (kappa)]
      [(IfThenElse test-exp then-exp else-exp)
       (let ([x (gensym! "join%")])
	 (make-LinBindJoin
	   level
	   (make-LinAbstraction
	     (list x)
	     (kappa))
	   (let ([new-level (+ level 1)])
	     (linearize-expression22
	       test-exp
	       new-level
	       (lambda ()
		 (linearize-expression1
		   then-exp
		   new-level
		   level))
	       (lambda ()
		 (linearize-expression1
		   else-exp
		   new-level
		   level))))))]
      [(Application e es)
       (case-record e
	 [(Abstraction xs b)
	  (linearize-expression2s
	    es
	    level
	    (lambda (vs level_vs)
	      (make-LinApplication
		(make-LinAbstraction
		  xs
		  (linearize-expression3 b level kappa))
		vs)))]
	 [else
	  (linearize-expression2
	    e
	    level
	    (lambda (v level_e)
	      (linearize-expression2s
		es
		level_e
		(lambda (vs level_es)
		  (make-LinSeq
		    level
		    (make-LinApplication v vs)
		    (kappa))))))])]
      [(SequenceExpr2 e es)
       (letrec ([traverse
		 (lambda (e es)
		   (if (null? es)
		       (linearize-expression3 e level kappa)
		       (linearize-expression3 e
					      level
					      (lambda ()
						(traverse (car es)
							  (cdr es))))))])
	 (traverse e es))]
      [(LetRecExpr xs ls b)
       (make-LinLetRecExpr
	 xs
	 (map1 (lambda (l)
		 (case-record l
		   [(Abstraction-val lam)
		    (make-Abstraction-val
		      (case-record lam
			[(Abstraction xs b)
			 (make-LinAbstraction xs
					      (linearize-expression0 b))]
			[else
			 (error 'linearize-expression1
				"invalid letrec header: ~s" l)]))]
		   [(Abstraction-box lam)
		    (make-Abstraction-box
		      (case-record lam
			[(Abstraction xs b)
			 (make-LinAbstraction xs
					      (linearize-expression0 b))]
			[else
			 (error 'linearize-expression1
				"invalid letrec header: ~s" l)]))]
		   [else
		    (error 'linearize-expression2
			   "invalid letrec header: ~s" l)]))
	       ls)
	 (linearize-expression3 b level kappa))]
      [(Assign x e)
       (linearize-expression2 e
			      level
			      (lambda (v new-level)
				(make-LinSeq
				  level
				  (make-LinAssign x v)
				  (kappa))))]
      [else
       (error 'linearize-expression3 "quasar: ~s" e)])))


;;;;;;;;;;
;;; 

(define linearize-definitions
  (lambda (ds)
    (letrec ([walk
	      (lambda (ds)
		(if (null? ds)
		    '()
		    (case-record (car ds)
		      [(DefExp name expression)
		       (cons (make-LinDefExp
			       name
			       (linearize-definiens expression name))
			     (walk (cdr ds)))]
		      [else
		       (error 'linearize-definitions
			      "not a definition: ~s" (car ds))])))])
      (walk ds))))

(define linearize-definiens
  (lambda (e x)
    (case-record e
      [(Literal l)
       (make-LinAssign x (make-LinLiteral l))]
      [(Variable xp)
       (make-LinAssign x (make-LinVariable xp))]
      [(Abstraction xs b)
       (make-LinAssignLam x
			  (make-LinAbstraction xs
					       (linearize-expression0 b)))]
      [(IfThenElse test-exp then-exp else-exp)
       (make-LinBindVal
	 0
	 (make-LinAbstraction
	   '()
	   (linearize-expression22
	     test-exp
	     0
	     (lambda ()
	       (linearize-expression0 then-exp))
	     (lambda ()
	       (linearize-expression0 else-exp))))
	 (make-LinAssignApp
	   x
	   (make-LinApplication (make-LinVariableTmp 0) '())))]
      [(Application e es)
       (case-record e
	 [(Abstraction xs b)
	  (linearize-expression2s
	    es
	    0
	    (lambda (vs level_vs)
	      (make-LinApplication
		(make-LinAbstraction
		  xs
		  (linearize-definiens b x))
		vs)))]
	 [else
	  (linearize-expression2
	    e
	    0
	    (lambda (v level_e)
	      (linearize-expression2s
		es
		level_e
		(lambda (vs level_es)
		  (make-LinAssignApp x (make-LinApplication v vs))))))])]
      [(SequenceExpr2 e es)
       (letrec ([traverse
		 (lambda (e es)
		   (if (null? es)
		       (linearize-definiens e x)
		       (linearize-expression3 e
					      0
					      (lambda ()
						(traverse (car es)
							  (cdr es))))))])
	 (traverse e es))]
      [(LetRecExpr xs ls b)
       (make-LinLetRecExpr
	 xs
	 (map1 (lambda (l)
		 (case-record l
		   [(Abstraction-val lam)
		    (make-Abstraction-val
		      (case-record lam
			[(Abstraction xs b)
			 (make-LinAbstraction xs
					      (linearize-expression0 b))]
			[else
			 (error 'linearize-expression1
				"invalid letrec header: ~s" l)]))]
		   [(Abstraction-box lam)
		    (make-Abstraction-box
		      (case-record lam
			[(Abstraction xs b)
			 (make-LinAbstraction xs
					      (linearize-expression0 b))]
			[else
			 (error 'linearize-expression1
				"invalid letrec header: ~s" l)]))]
		   [else
		    (error 'linearize-expression2
			   "invalid letrec header: ~s" l)]))
	       ls)
	 (linearize-definiens b x))]
      [(Assign y e)
       (linearize-expression2
	 e
	 0
	 (lambda (v new-level)
	   (make-LinBindCmp
	     0
	     (make-LinAssign y v)
	     (make-LinAssign x (make-LinVariableTmp 0)))))]
      [else
       (error 'linearize-definiens "quasar: ~s" e)])))


;;;;;;;;;;
;;;

(define linearize-program
  (lambda (p)
    (case-record p
      [(Boxed-Program the-definitions the-expression)
       (begin
	 (warn "Linearizing")
	 (let* ([ds (linearize-definitions the-definitions)]
		[e (linearize-expression0 the-expression)])
	   (begin
	     (warnl " / Linearized")
	     (make-Linearized-Program ds e))))]
      [else
       (error 'linearized-program "not a boxed program")])))


;;;;;;;;;;

;;; end of "3linearizer.scm"
