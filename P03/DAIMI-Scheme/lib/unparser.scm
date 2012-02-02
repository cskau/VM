;;; DAIMI-Scheme/lib/unparser.scm
;;; an unparser for desugared (as well as unboxed) DAIMI-Scheme programs
;;; Olivier Danvy <danvy@brics.dk>
;;; October 2002, October 2003


;;;;;;;;;;
;;;

(define unparse-expression
  (lambda (e)
    (case-record e
      [(Literal l)
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
	 [else
	  (error 'unparse-expression "Illegal literal: ~s" l)])]
      [(Variable x)
       x]
      [(Abstraction xs b)
       `(lambda ,xs ,(unparse-expression b))]
      [(Boxed-Abstraction xs b mutated)
       `(lambda ,xs
	  ,(if (null? mutated)
	       (unparse-expression b)
	       `(let (,(map (lambda (x)
			      `[,x (vector ,x)])
			    mutated))
		  ,(unparse-expression b))))]
      [(IfThenElse test-exp then-exp else-exp)
       `(if ,(unparse-expression test-exp)
	    ,(unparse-expression then-exp)
	    ,(unparse-expression else-exp))]
      [(Application e es)
       `(,(unparse-expression e) ,@(map unparse-expression es))]
      [(SequenceExpr2 e es)
       `(begin ,(unparse-expression e) ,@(map unparse-expression es))]
      [(CondExpr ts es)
       (error 'unparse-expression "cond expression")]
      [(CaseExpr e ls es)
       (error 'unparse-expression "case expression")]
      [(CaseRecordExpr e ls es)
       (error 'unparse-expression "case-record expression")]
      [(ConjExpr es)
       (error 'unparse-expression "and expression")]
      [(DisjExpr es)
       (error 'unparse-expression "or expression")]
      [(LetExpr xs es b)
       (error 'unparse-expression "let expression")]
      [(SLetExpr xs es b)
       (error 'unparse-expression "let* expression")]
      [(LetRecExpr xs ls b)
       `(letrec ,(map (lambda (x l)
			(list x
			      (case-record l
				[(Abstraction-val lam)
				 (unparse-expression lam)]
				[(Abstraction-box lam)
				 (unparse-expression lam)]
				[else
				 (unparse-expression l)])))
		      xs
		      ls)
	  ,(unparse-expression b))]
      [(Suspend e)
       (error 'unparse-expression "suspend expression")]
      [(Delay e)
       (error 'unparse-expression "delay expression")]
      [(Assign x e)
       `(set! ,x ,(unparse-expression e))]
      [else
       (error 'unparse-expression "quasar: ~s" e)])))

;;;;;;;;;;
;;;

(define unparse-program
  (lambda (p)
    (case-record p
      [(Desugared-Program the-definitions the-expression)
       (append (map (lambda (d)
		      (case-record d
			[(DefExp name expression)
			 `(define ,name ,(unparse-expression expression))]
			[else
			 (error 'unparse-program
				"not a definition: ~s" d)]))
		    the-definitions)
	       (list (unparse-expression the-expression)))]
      [(Boxed-Program the-definitions the-expression)
       (unparse-program (make-Desugared-Program the-definitions the-expression))]
      [else
       (error 'unparse-program "not a desugared/boxed program")])))


;;;;;;;;;;

;;; end of "unparser.scm"
