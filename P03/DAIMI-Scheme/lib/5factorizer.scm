;;; DAIMI-Scheme/lib/factorizer.scm
;;; a factorizer for scoped DAIMI-Scheme programs
;;; Olivier Danvy <danvy@brics.dk>
;;; October 2002, October 2003


;;;;;;;;;;
;;;

(define-record (Factorized-Program the-definitions
				   the-expression
				   the-lambda-headers
				   the-lambda-bodies))

(define-record (FacDefExp x e))

(define-record (FacLiteral l))
(define-record (FacVariable x kind offset))
(define-record (FacVariableTmp int))
(define-record (FacVariableJoin int))
(define-record (FacAbstraction-stay xs body))
(define-record (FacAbstraction-deep index-in-global-table-of-lambdas))
(define-record (FacAbstraction-flat index-in-global-table-of-lambdas freevars))

(define-record (FacIfThenElse test-exp then-exp else-exp))
(define-record (FacApplication v vs))
(define-record (FacLetRecExpr xs ls b))
(define-record (FacAssign int v))
(define-record (FacAssignLam int l))
(define-record (FacAssignApp int c))
(define-record (FacBindCmp int c b))
(define-record (FacBindVal int v b))
(define-record (FacReturn v))
(define-record (FacBindJoin int c b))
(define-record (FacSeq i c1 c2))

(define-record (FacInteger integer))
(define-record (FacBoolean boolean))
(define-record (FacString string))
(define-record (FacCharacter character))
(define-record (FacSymbol symbol))
(define-record (FacNil))
(define-record (FacVoid))


;;;;;;;;;;
;;;

(define factorize-expression
  (lambda (e ilam lams k)
    (case-record e
      [(ScoIfThenElse test-exp then-exp else-exp)
       (factorize-value
	 test-exp
	 ilam lams
	 (lambda (test-exp ilam lams)
	   (factorize-expression
	     then-exp
	     ilam lams
	     (lambda (then-exp ilam lams)
	       (factorize-expression
		 else-exp
		 ilam lams
		 (lambda (else-exp ilam lams)
		   (k (make-FacIfThenElse test-exp then-exp else-exp)
		      ilam lams)))))))]
      [(ScoApplication v vs)
       (case-record v
	 [(ScoAbstraction-deep xs b)
	  (factorize-values
	    vs
	    ilam lams
	    (lambda (vs ilam lams)
	      (factorize-expression
		b
		ilam lams
		(lambda (b ilam lams)
		  (k (make-FacApplication
		       (make-FacAbstraction-stay xs b)
		       vs)
		     ilam lams)))))]
	 [else
	  (factorize-value
	    v
	    ilam lams
	    (lambda (v ilam lams)
	      (factorize-values
		vs
		ilam lams
		(lambda (vs ilam lams)
		  (k (make-FacApplication v vs)
		     ilam lams)))))])]
      [(ScoLetRecExpr xs ls b)
       (factorize-expression
	 b
	 (+ ilam (length xs))
	 (letrec ([append-reverse
		   (lambda (ls lams)
		     (if (null? ls)
			 lams
			 (append-reverse
			   (cdr ls)
			   (cons (vector-ref (car ls) 1) lams))))])
	   (append-reverse ls lams))
	 (lambda (b new-ilam new-lams)
	   (k (make-FacLetRecExpr
		xs
		(letrec ([make-list-of-lambdas
			  (lambda (ls ilam)
			    (if (null? ls)
				'()
				(cons
				  (vector (vector-ref (car ls) 0) ilam)
				  (make-list-of-lambdas
				    (cdr ls)
				    (+ ilam 1)))))])
		  (make-list-of-lambdas ls ilam))
		b)
	      new-ilam new-lams)))]
      [(ScoAssign x v)
       (factorize-value
	 v
	 ilam lams
	 (lambda (v ilam lams)
	   (k (make-FacAssign x v)
	      ilam lams)))]
      [(ScoAssignLam x v)
       (factorize-value
	 v
	 ilam lams
	 (lambda (v ilam lams)
	   (k (make-FacAssignLam x v)
	      ilam lams)))]
      [(ScoAssignApp x c)
       (factorize-expression
	 c
	 ilam lams
	 (lambda (c ilam lams)
	   (k (make-FacAssignApp x c)
	      ilam lams)))]
      [(ScoBindCmp i c b)
       (factorize-named
	 c
	 ilam lams
	 (lambda (c ilam lams)
	   (factorize-expression
	     b
	     ilam lams
	     (lambda (b ilam lams)
	       (k (make-FacBindCmp i c b)
		  ilam lams)))))]
      [(ScoBindVal i v b)
       (factorize-value
	 v
	 ilam lams
	 (lambda (v ilam lams)
	   (factorize-expression
	     b
	     ilam lams
	     (lambda (b ilam lams)
	       (k (make-FacBindVal i v b)
		  ilam lams)))))]
      [(ScoReturn v)
       (factorize-value
	 v
	 ilam lams
	 (lambda (v ilam lams)
	   (k (make-FacReturn v)
	      ilam lams)))]
      [(ScoBindJoin i l b)
       (factorize-expression
	 b
	 (+ ilam 1)
	 (cons l lams)
	 (lambda (b new-ilam new-lams)
	   (k (make-FacBindJoin
		i
		(make-FacAbstraction-deep ilam)
		b)
	      new-ilam new-lams)))]
      [(ScoSeq i c1 c2)
       (factorize-named
	 c1
	 ilam lams
	 (lambda (c1 ilam lams)
	   (factorize-expression
	     c2
	     ilam lams
	     (lambda (c2 ilam lams)
	       (k (make-FacSeq i c1 c2)
		  ilam lams)))))]
      [else
       (error 'factorize-expression "quasar: ~s" e)])))

(define factorize-value
  (lambda (v ilam lams k)
    (case-record v
      [(ScoLiteral l)
       (case-record l
	 [(Integer i)
	  (k (make-FacLiteral (make-FacInteger i))
	     ilam lams)]
	 [(Boolean b)
	  (k (make-FacLiteral (make-FacBoolean b))
	     ilam lams)]
	 [(String s)
	  (k (make-FacLiteral (make-FacString s))
	     ilam lams)]
	 [(Character c)
	  (k (make-FacLiteral (make-FacCharacter c))
	     ilam lams)]
	 [(Symbol s)
	  (k (make-FacLiteral (make-FacSymbol s))
	     ilam lams)]
	 [(Nil)
	  (k (make-FacLiteral (make-FacNil))
	     ilam lams)]
	 [(Void)
	  (k (make-FacLiteral (make-FacVoid))
	     ilam lams)]
	 [else
	  (error 'factorize-value
		 "quasar: ~s" l)])]
      [(ScoVariable x kind offset)
       (k (make-FacVariable x kind offset)
	  ilam lams)]
      [(ScoVariableTmp i)
       (k (make-FacVariableTmp i)
	  ilam lams)]
      [(ScoVariableJoin i)
       (k (make-FacVariableJoin i)
	  ilam lams)]
      [(ScoAbstraction-flat xs b freevars)
       (k (make-FacAbstraction-flat ilam freevars)
	  (+ ilam 1)
	  (cons v lams))]
      [(ScoAbstraction-deep xs b)
       (k (make-FacAbstraction-deep ilam)
	  (+ ilam 1)
	  (cons v lams))]
      [else
       (error 'factorize-value "quasar: ~s" v)])))

(define factorize-values
  (lambda (vs ilam lams k)
    (if (null? vs)
	(k '() ilam lams)
	(factorize-value
	  (car vs)
	  ilam lams
	  (lambda (v ilam lams)
	    (factorize-values
	      (cdr vs)
	      ilam lams
	      (lambda (vs ilam lams)
		(k (cons v vs)
		   ilam lams))))))))

(define factorize-named
  (lambda (c ilam lams k)
    (case-record c
      [(ScoApplication v vs)
       (factorize-value
	 v
	 ilam lams
	 (lambda (v ilam lams)
	   (factorize-values
	     vs
	     ilam lams
	     (lambda (vs ilam lams)
	       (k (make-FacApplication v vs)
		  ilam lams)))))]
      [(ScoAssign x v)
       (factorize-value
	 v
	 ilam lams
	 (lambda (v ilam lams)
	   (k (make-FacAssign x v)
	      ilam lams)))]
      [(ScoAbstraction-flat xs b freevars)
       (k (make-FacAbstraction-flat ilam freevars)
	  (+ ilam 1)
	  (cons c lams))]
      [else
       (error 'factorize-named
	      "not a named computation: ~s" c)])))


;;;;;;;;;;
;;;

(define factorize-definitions
  (lambda (ds ilam lams k)
    (letrec ([walk
	      (lambda (ds ilam lams a)
		(if (null? ds)
		    (k a ilam lams)
		    (case-record (car ds)
		      [(ScoDefExp x e)
		       (factorize-expression
			 e ilam lams
			 (lambda (e ilam lams)
			   (walk (cdr ds)
				 ilam lams
				 (cons (make-FacDefExp x e) a))))]
		      [else
		       (error 'factorize-definitions
			      "not a scoped definition: ~s"
			      (car ds))])))])
      (walk ds ilam lams '()))))


;;;;;;;;;;
;;;

(define factorize-finalize
  (lambda (ilam lams k)
    (letrec ([iterate
	      (lambda (index ls a ilam lams rest)
		(if (null? ls)
		    (if (null? lams)
			(if (= index ilam)
			    (k ilam a rest)
			    (error 'factorize-finalize
				   "mismatching indices: ~s and ~s"
				   index ilam))
			(iterate index
				 (reverse lams)
				 a
				 ilam
				 '()
				 rest))
		    (case-record (car ls)
		      [(ScoAbstraction-flat xs b freevars)
		       (factorize-expression
			 b
			 ilam lams
			 (lambda (b ilam lams)
			   (let ([label (string->symbol
					  (string-append
					    "lambda-flat%"
					    (integer->string
					      index)))])
			     (iterate
			       (+ index 1)
			       (cdr ls)
			       (cons (list (if (symbol? xs)
					       -1
					       (length xs))
					   label)
				     a)
			       ilam lams
			       (cons (cons label b) rest)))))]
		      [(ScoAbstraction-deep xs b)
		       (factorize-expression
			 b
			 ilam lams
			 (lambda (b ilam lams)
			   (let ([label (string->symbol
					  (string-append
					    "lambda-deep%"
					    (integer->string
					      index)))])
			     (iterate
			       (+ index 1)
			       (cdr ls)
			       (cons (list (if (symbol? xs)
					       -1
					       (length xs))
					   label)
				     a)
			       ilam lams
			       (cons (cons label b) rest)))))]
		      [else
		       (error 'factorize-program
			      "not a lambda-abstraction: ~s"
			      (car ls))])))])
      (iterate 0 (reverse lams) '() ilam '() '()))))


;;;;;;;;;;
;;;

(define factorize-program
  (lambda (p)
    (case-record p
      [(Scoped-Program the-definitions the-expression)
       (begin
	 (warn "Factorizing")
	 (factorize-definitions
	   the-definitions 0 '()
	   (lambda (ds ilam lams)
	     (factorize-expression
	       the-expression ilam lams
	       (lambda (e ilam lams)
		 (factorize-finalize
		   ilam lams
		   (lambda (ilam lams rest)
		     (begin
		       (warnl " / Factorized")
		       (make-Factorized-Program
			 (reverse ds)
			 e
			 (reverse lams)
			 (reverse rest))))))))))]
      [else
       (error 'factorize-program "not a scoped program")])))


;;;;;;;;;;

;;; end of "factorizer.scm"
