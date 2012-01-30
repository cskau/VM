;;; ae.scm
;;; Scheme code for the VM course, 24 January 2012
;;; Olivier Danvy <danvy@cs.au.dk>

;;;;;;;;;;

;;; <expression> ::= <integer> | (<operator> <expression> <expression>)
;;;   <operator> ::= + | - | *

(define ae-integer?
  number?)

(define ae-operator?
  (lambda (v)
    (member v '(+ - *))))

(define ae-operation?
  (lambda (ae)
    (and (pair? ae)
	 (ae-operator? (car ae))
	 (let ([ae2 (cdr ae)])
	   (and (pair? ae2)
		(let ([ae3 (cdr ae2)])
		  (and (pair? ae3)
		       (null? (cdr ae3)))))))))

(define ae-interpret
  (lambda (ae)
    (letrec ([ae-apply
	      (lambda (rator n1 n2)
		(case rator
		  [(+)
		   (+ n1 n2)]
		  [(-)
		   (- n1 n2)]
		  [(*)
		   (* n1 n2)]
		  [else
		   (errorf 'ae-interpret "unknown operator: ~s" rator)]))]
	     [ae-eval
	      (lambda (ae)
		(cond
		 [(ae-integer? ae)
		  ae]
		 [(ae-operation? ae)
		  (ae-apply (list-ref ae 0)
			    (ae-eval (list-ref ae 1))
			    (ae-eval (list-ref ae 2)))]
		 [else
		  (errorf 'ae-interpret "unknown expression: ~s" ae)]))])
      (ae-eval ae))))

;;;;;;;;;;

;;; <instruction> ::= (push <value>) | (add) | (sub) | (mul)
;;;     <program> ::= {<instruction>}*

(define ae-push
  (lambda (v)
    (list 'push v)))

(define ae-push?
  (lambda (e)
    (and (pair? e)
	 (equal? (car e) 'push)
	 (let ([e (cdr e)])
	   (and (pair? e)
		(null? (cdr e)))))))

(define ae-add
  '(add))

(define ae-add?
  (lambda (e)
    (equal? e ae-add)))

(define ae-sub
  '(sub))

(define ae-sub?
  (lambda (e)
    (equal? e ae-sub)))

(define ae-mul
  '(mul))

(define ae-mul?
  (lambda (e)
    (equal? e ae-mul)))

(define ae-compile
  (lambda (ae)
    (letrec ([ae-translate
	      (lambda (ae)
		(cond
		 [(ae-integer? ae)
		  (list (list 'push ae))]
		 [(ae-operation? ae)
		  (append (ae-translate (list-ref ae 1))
			  (ae-translate (list-ref ae 2))
			  (case (list-ref ae 0)
			    [(+)
			     (list ae-add)]
			    [(-)
			     (list ae-sub)]
			    [(*)
			     (list ae-mul)]
			    [else
			     (errorf 'ae-compile
				     "unknown operator: ~s"
				     (list-ref ae 0))]))]
		 [else
		  (errorf 'ae-compile "unknown expression: ~s" ae)]))])
      (ae-translate ae))))

(define ae-push
  cons)

(define ae-pop
  (lambda (s k)
    (if (pair? s)
	(k (car s) (cdr s))
	(errorf 'ae-pop "ill-formed stack: ~s" s))))

(define ae-run
  (lambda (p)
    (letrec ([ae-decode-execute
	      (lambda (i s)
		(cond
		 [(ae-push? i)
		  (ae-push (cadr i) s)]
		 [(ae-add? i)
		  (ae-pop s (lambda (n1 s1)
			      (ae-pop s1 (lambda (n2 s2)
					   (ae-push (+ n1 n2) s2)))))]
		 [(ae-sub? i)
		  (ae-pop s (lambda (n1 s1)
			      (ae-pop s1 (lambda (n2 s2)
					   (ae-push (- n1 n2) s2)))))]
		 [(ae-mul? i)
		  (ae-pop s (lambda (n1 s1)
			      (ae-pop s1 (lambda (n2 s2)
					   (ae-push (* n1 n2) s2)))))]
		 [else
		  (errorf 'ae-run "unknown instruction: ~s" i)]))]
	     [loop
	      (lambda (is s)
		(if (null? is)
		    s
		    (loop (cdr is) (ae-decode-execute (car is) s))))])
      (loop p '()))))

(define ae-test
  (lambda (s msg)
    (let* ([v (ae-interpret s)]
	   [t (ae-compile s)]
	   [w (let ([r (ae-run t)])
		(if (and (pair? r)
			 (null? (cdr r)))
		    (car r)
		    (errorf msg "unexpected result for compiled code")))])
      (or (equal? v w)
	  (errorf msg "inconsistent results: ~s and ~s" v w)))))

(define z1
  (ae-test '(* 3 (+ 2 4))
	   'z1))

(define z2
  (ae-test '(* (+ 20 40) (+ 2 4))
	   'z2))

;;;;;;;;;;


;;; Exercise 1:
;;; 
;;; The virtual machine contains a bug.  Find it and fix it.
;;; 
;;; 
;;; 
;;; Exercise 2:
;;; 
;;; The compiler uses list concatenation (ie, append).
;;; Rewrite it so that it does not use append but cons.
;;; (Hint: use an accumulator.)
;;; 
;;; 
;;; 
;;; Exercise 3:
;;; 
;;; Extend the source syntax and the target syntax with division,
;;; and then the interpreter, the compiler, and the virtual machine.
;;; 
;;; 
;;; Exercises 4 and 5 are to be made independently of each other.
;;; 
;;; 
;;; Exercise 4:
;;; 
;;; Extend the source syntax with a conditional expression
;;; and the interpreter
;;; so as evaluating
;;;   (if0 0 e1 e2)
;;; will lead to e1 being evaluated,
;;; and evaluating
;;;   (if0 v e1 e2)
;;; will lead to e2 being evaluated if v is not 0.
;;; 
;;; Extend the target syntax, the compiler, and the virtual machine
;;; to conditional expressions.
;;; (Hint: you will need to add labels.)
;;; 
;;; 
;;; Exercise 5:
;;; 
;;; Extend the source syntax with booleans,
;;; and add a comparison operator.
;;; 
;;; 
;;; Exercise 6 (the combination of Exercises 4 and 5):
;;; 
;;; Extend the source syntax with booleans,
;;; a comparison operator, and
;;; a boolean conditional expression.
;;; 
;;; Then extend the interpreter, the compiler, and the virtual machine.

;;;;;;;;;;

;;; end of ae.scm
