;;; DAIMI-Scheme/lib/ditcher.scm
;;; an unparser for linearized DAIMI-Scheme programs
;;; (the last stop in Scheme land)
;;; Olivier Danvy <danvy@brics.dk>
;;; October-November 2002, October 2003


;;;;;;;;;;
;;;

(define ditch-value
  (lambda (e)
    (case-record e
      [(LinLiteral l)
       (case-record l
	 [(Integer lit)
	  lit]
	 [(Boolean lit)
	   lit]
	 [(String lit)
	  lit]
	 [(Character lit)
	   lit]
	 [(Symbol lit)
	  `',lit]
	 [(Nil)
	  ''()]
	 [(Void)
	  (let ([x '()]) (set! x '()))]
	 [else
	  (error 'ditch-expression "Illegal literal: ~s" l)])]
      [(LinVariable x)
       x]
      [(LinVariableTmp i)
       (string->symbol (string-append "tmp%" (integer->string i)))]
      [(LinVariableJoin i)
       (string->symbol (string-append "tmp%" (integer->string i)))]
      [(LinAbstraction xs b)
       `(lambda ,xs ,(ditch-expression b))]
      [else
       (error 'ditch-value "quasar: ~s" e)])))

(define ditch-expression
  (lambda (e)
    (case-record e
      [(LinIfThenElse test-exp then-exp else-exp)
       `(if ,(ditch-value test-exp)
	    ,(ditch-expression then-exp)
	    ,(ditch-expression else-exp))]
      [(LinApplication e es)
       ;;; `(,(ditch-value e) ,@(map ditch-value es))
       (ditch-application e (map ditch-value es))]
      [(LinLetRecExpr xs ls b)
       `(letrec ,(map (lambda (x l)
			(list x (case-record l
				  [(Abstraction-val lam)
				   (ditch-value lam)]
				  [(Abstraction-box lam)
				   `(vector
				      ,(ditch-value lam))]
				  [else
				   (error 'ditch-expression
					  "not a lambda: ~s" l)])))
		      xs
		      ls)
	  ,(ditch-expression b))]
      [(LinAssign x v)
       `(set! ,x ,(ditch-value v))]
      [(LinAssignLam x l)
       `(set! ,x ,(ditch-value l))]
      [(LinAssignApp x c)
       `(set! ,x ,(ditch-expression c))]
      [(LinBindCmp i e b)
       (ditch-let*
	 (string->symbol (string-append "tmp%" (number->string i)))
	 (ditch-named e)
	 (ditch-expression b))]
      [(LinBindVal i v b)
       (ditch-let*
	 (string->symbol (string-append "tmp%" (number->string i)))
	 (ditch-value v)
	 (ditch-expression b))]
      [(LinReturn v)
       (ditch-value v)]
      [(LinBindJoin i l b)
       (ditch-let*
	 (string->symbol (string-append "tmp%" (number->string i)))
	 (ditch-value l)
	 (ditch-expression b))]
      [(LinSeq i e b)
       (ditch-let*
	 (string->symbol (string-append "tmp%" (number->string i)))
	 (ditch-named e)
	 (ditch-expression b))]
      [else
       (error 'ditch-expression "quasar: ~s" e)])))

(define ditch-application
  (lambda (raw-e cooked-es)
    (case-record raw-e
      [(LinAbstraction xs b)
       (if (symbol? xs)
	   (ditch-let* xs
		       `(list ,@cooked-es)
		       (ditch-expression b))
	   (let ([arity (length xs)])
	     (cond
	       [(not (= arity (length cooked-es)))
		(error 'ditch-application
		       "mismatching arities: ~s and ~s"
		       (ditch-expression raw-e)
		       cooked-es)]
	       [(= arity 1)
		(ditch-let* (car xs)
			    (car cooked-es)
			    (ditch-expression b))]
	       [(= arity 0)
		(ditch-expression b)]
	       [else
		`(let ,(map list xs cooked-es)
		   ,(ditch-expression b))])))]
      [else
       `(,(ditch-value raw-e) ,@cooked-es)])))

(define ditch-let*
  (lambda (x h b)
    (if (and (pair? b)
	     (eqv? (car b) 'let*))
	`(let* ([,x ,h] ,@(cadr b))
	   ,(caddr b))
	`(let* ([,x ,h])
	   ,b))))

(define ditch-named
  (lambda (c)
    (case-record c
      [(LinApplication e es)
       (ditch-application e (map ditch-value es))]
      [(LinAssign x v)
       `(set! ,x ,(ditch-value v))]
      [(LinAbstraction xs b)
       `(lambda ,xs ,(ditch-expression b))]
      [else
       (error 'ditch-named "not a computation: ~s" c)])))
		  

;;;;;;;;;;
;;;

(define ditch-program
  (lambda (p)
    (case-record p
      [(Linearized-Program the-definitions the-expression)
       (if (null? the-definitions)
	   (ditch-expression the-expression)
	   `(begin
	      ,@(map (lambda (d)
		       (case-record d
			 [(LinDefExp name expression)
			  (ditch-expression expression)]
			 [else
			  (error 'ditch-program
				 "not a definition: ~s" d)]))
		     the-definitions)
	      ,(ditch-expression the-expression)))]
      [else
       (error 'ditch-program "not a linearized program")])))


;;;;;;;;;;

;;; end of "ditcher.scm"
