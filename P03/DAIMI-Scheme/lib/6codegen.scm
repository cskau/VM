;;; DAIMI-Scheme/lib/codegen.scm
;;; a code generator for factorized DAIMI-Scheme programs
;;; Olivier Danvy <danvy@brics.dk>
;;; October 2002, October 2003


;;;;;;;;;;
;;;

(define-record (Compiled-Program number-of-global-definitions
				 number-of-temporaries
				 number-of-results
			         the-lambdas
				 the-code
				 the-tag))


;;;;;;;;;;
;;;

(define codegen-abstraction-flat
  (lambda (freevars a)
    (letrec ([walk
	      (lambda (xs i)
		(if (null? xs)
		    a
		    (cons (case-record (car xs)
			    [(ScoVariable x lex offset)
			     (list 'move lex offset 'vec i)]
			    [else
			     (error 'codegen-abstraction-flat
				    "illegal free variable: ~s"
				    (car xs))])
			  (walk (cdr xs) (+ i 1)))))])
      (walk freevars 0))))

(define codegen-application
  (lambda (vs a)
    (letrec ([walk
	      (lambda (vs j)
		(if (null? vs)
		    a
		    (codegen-value
		      (car vs)
		      (lambda (what S i)
			(cons (list what S i 'vec j)
			      (walk (cdr vs) (+ j 1)))))))])
      (walk vs 0))))

(define codegen-letrec
  (lambda (ls a)
    (letrec ([walk
	      (lambda (ls i)
		(if (null? ls)
		    a
		    (case-record (car ls)
		      [(Abstraction-val j)
		       (cons (list 'load 'close-deep j 0 i)
			     (walk (cdr ls) (+ i 1)))]
		      [(Abstraction-box j)
		       (let ([label (gensym! "letrec%")])
			 (cons
			   '(new-vec 1)
			   (cons
			     (list 'load 'close-deep j 'vec 0)
			     (cons
			       (list 'call
				       'lib
				       (ref-list-kind predefined-procedures
						      'vector)
				       0)
			       (cons
				 (list 'move 'res 0 0 i)
				 (walk (cdr ls) (+ i 1)))))))]
		      [else
		       (error 'codegen-letrec
			      "not a deep abstraction: ~s"
			      (car ls))])))])
      (walk ls 0))))

(define codegen-expression
  (lambda (e)
    (case-record e
      [(FacIfThenElse test-exp then-exp else-exp)
       (let ([else-label (gensym! "else%")])
	 (codegen-value
	   test-exp
	   (lambda (what S i)
	     (cons
	       (list 'jump-if-false S i else-label)
	       (append
		 (codegen-expression then-exp)
		 (cons
		   (list 'label else-label)
		   (codegen-expression else-exp)))))))]
      [(FacApplication v vs)
       (cons
	 (list 'new-vec (length vs))
	 (case-record v
	   [(FacAbstraction-stay xs b)
	    (codegen-application
	      vs
	      (cons
		'(extend)
		(codegen-expression b)))]
	   [else
	    (codegen-application
	      vs
	      (codegen-value
		v
		(lambda (what S i)
		  (list (list 'tail-call S i)))))]))]
      [(FacLetRecExpr xs ls b)
       (cons
	 (list 'new-vec (length xs))
	 (cons
	   '(extend)
	   (codegen-letrec
	     ls
	     (codegen-expression b))))]
      [(FacAssign x v)
       (codegen-value v
		      (lambda (what S i)
			(list (list what S i (car x) (cdr x)))))]
      [(FacAssignLam x v)
       (codegen-value v
		      (lambda (what S i)
			(cons
			  (list what S i (car x) (cdr x))
			  '())))]
      [(FacAssignApp x c)
       (case-record c
	 [(FacApplication v vs)
	  (cons
	    (list 'new-vec (length vs))
	    (codegen-application
	      vs
	      (codegen-value
		v
		(lambda (what S i)
		  (cons
		    (list 'call S i 0)
		    (cons
		      (list 'move 'res 0 (car x) (cdr x))
		      '()))))))]
	 [else
	  (error 'codegen-expression
		 "not an assigned application: ~s" c)])]
      [(FacBindCmp i c b)
       (codegen-named c
		      i
		      (codegen-expression b))]
      [(FacBindVal j v b)
       (codegen-value v
		      (lambda (what S i)
			(cons (list what S i 'tmp j)
			      (codegen-expression b))))]
      [(FacReturn v)
       (codegen-value v
		      (lambda (what S i)
			(list (list what S i 'res 0)
			      '(return))))]
      [(FacBindJoin i v b)
       (case-record v
	 [(FacAbstraction-deep offset)
	  (cons (list 'load 'close-deep offset 'tmp i)
		(codegen-expression b))]
	 [else
	  (error 'codegen-expression
		 "not a deep abstraction: ~s" v)])]
      [(FacSeq j c1 c2)
       (case-record c1
	 [(FacApplication v vs)
	  (cons
	    (list 'new-vec (length vs))
	    (codegen-application
	      vs
	      (codegen-value
		v
		(lambda (what S i)
		  (cons
		    (list 'call S i j)
		    (codegen-expression c2))))))]
	 [(FacAssign x v)
	  (codegen-value
	    v
	    (lambda (what S i)
	      (cons
		(list what S i (car x) (cdr x))
		(codegen-expression c2))))]
	 [else
	  (error 'codegen-expression
		 "not a sequenced computation: ~s" c1)])]
      [else
       (error 'codegen-expression "quasar: ~s" e)])))

(define codegen-value
  (lambda (v k)
    (case-record v
      [(FacLiteral l)
       (case-record l
	 [(FacInteger i)
	  (k 'load 'int i)]
	 [(FacBoolean b)
	  (k 'load 'bool (if b 1 0))]
	 [(FacString s)
	  (k 'load 'str s)]
	 [(FacCharacter c)
	  (k 'load 'char (char->integer c))]
	 [(FacSymbol s)
	  (k 'load 'sym s)]
	 [(FacNil)
	  (k 'load 'nil '_)]
	 [(FacVoid)
	  (k 'load 'void '_)]
	 [else
	  (error 'codegen-value
		 "quasar: ~s" l)])]
      [(FacVariable x kind offset)
       (k 'move kind offset)]
      [(FacVariableTmp i)
       (k 'move 'tmp i)]
      [(FacVariableJoin i)
       (k 'move 'tmp i)]
      [(FacAbstraction-flat offset freevars)
       (cons
	 (list 'new-vec (length freevars))
	 (codegen-abstraction-flat
	   freevars
	   (k 'load 'close-flat offset)))]
      [(FacAbstraction-deep offset)
       (k 'load 'close-deep offset)]
      [else
       (error 'codegen-value "quasar: ~s" v)])))

(define codegen-named
  (lambda (c j a)
    (case-record c
      [(FacApplication v vs)
       (cons
	 (list 'new-vec (length vs))
	 (codegen-application
	   vs
	   (codegen-value
	     v
	     (lambda (what S i)
	       (cons
		 (list 'call S i j)
		 (if (and (not (null? a))
			  (let ([q (car a)])
			    (let ([q1 (car q)]
				  [q (cdr q)])
			      (and (eqv? q1 'move)
				   (let ([q2 (car q)])
				     (and (eqv? q2 'tmp)
					  (eqv? (car (cdr q)) j)))))))
		     (cons
		       (list 'move 
			     'res
			     0
			     (list-ref (car a) 3)
			     (list-ref (car a) 4))
		       (cdr a))
		     (cons
		       (list 'move 'res 0 'tmp j)
		       a)))))))]
      [(FacAssign x v)
       (codegen-value
	 v
	 (lambda (what S i)
	   (cons (list what S i (car x) (cdr x))
		 (cons (list 'load 'void '_ 'tmp j)
		       a))))]
      [(FacAbstraction-flat offset freevars)
       (cons (list 'new-vec (length freevars))
	     (codegen-abstraction-flat
	       freevars
	       (cons (list 'load 'close-flat offset 'tmp j)
		     a)))]
      [else
       (error 'codegen-named
	      "not a named computation: ~s" c)])))

(define codegen-definitions
  (lambda (ds)
    (map-append1 (lambda (d)
		   (case-record d
		     [(FacDefExp x e)
		      (codegen-expression e)]
		     [else
		      (error 'codegen-definitions
			     "not a factorized definition: ~s"
			     d)]))
		 ds)))

(define codegen-bodies
  (lambda (bodies)
    (map-append1 (lambda (body)
		   (cons (list 'label (car body))
			 (codegen-expression (cdr body))))
		 bodies)))


;;;;;;;;;;
;;;

(define how-many-tmp
  (lambda (is)
    (letrec ([walk (lambda (is n)
		     (if (null? is)
			 (1+ n)
			 (let ([i (car is)])
			   (if (and (or (eqv? (car i) 'move)
					(eqv? (car i) 'load))
				    (eqv? (cadddr i) 'tmp))
			       (walk (cdr is) (max n (caddddr i)))
			       (walk (cdr is) n)))))])
      (walk is -1))))

(define codegen-program
  (lambda (p)
    (case-record p
      [(Factorized-Program ds e lambda-headers lambda-bodies)
       (begin
	 (warn "Generating code")
	 (let* ([code-ds (codegen-definitions ds)]
		[code-e (codegen-expression e)]
		[code-bodies (codegen-bodies lambda-bodies)])
	   (begin
	     (warnl " / Code generated")
	     (let ([code (append code-ds code-e code-bodies)])
	       (make-Compiled-Program
		 (length ds)
		 (how-many-tmp code) 
		 1
		 lambda-headers
		 code
		 the-tag)))))]
      [else
       (error 'codegen-program "not a factorized program")])))


;;;;;;;;;;

;;; end of "codegen.scm"
