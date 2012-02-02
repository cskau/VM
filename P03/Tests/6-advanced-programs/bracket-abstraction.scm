;;; bracket-abstraction.scm
;;; from lambda-terms to combinators

;;;;;;;;;;

(define-record (Var x))
(define-record (Lam x e))
(define-record (App e0 e1))

;;;;;;;;;;

(define parse
  (lambda (e)
    (cond
      [(symbol? e)
       (make-Var e)]
      [(pair? e)
       (if (eqv? (car e) 'lambda)
	   (parse-lambda (car (cdr e)) (parse (car (cdr (cdr e)))))
	   (parse-apply (parse (car e)) (cdr e)))]
      [else
       (exit 'parse)])))

(define parse-lambda
  (lambda (xs e)
    (letrec ([traverse (lambda (xs)
			 (if (null? xs)
			     e
			     (make-Lam (car xs)
				       (traverse (cdr xs)))))])
      (traverse xs))))

(define parse-apply
  (lambda (e es)
    (letrec ([traverse (lambda (e es)
			 (if (null? es)
			     e
			     (traverse (make-App e (parse (car es)))
				       (cdr es))))])
      (traverse e es))))

(define list
  (lambda xs
    xs))

(define unparse
  (lambda (e)
    (case-record e
      [(Var x)
       x]
      [(Lam x e)
       (list 'lambda (list x) (unparse e))]
      [(App e0 e1)
       (list (unparse e0) (unparse e1))]
      [else
       (exit 'unparse)])))

;;;;;;;;;;

(define ba
  (lambda (e)
    (case-record e
      [(Var x)
       e]
      [(Lam x e)
       (ba-var x e)]
      [(App e0 e1)
       (make-App (ba e0) (ba e1))]
      [else
       (exit 'ba)])))

(define ba-var
  (lambda (x e)
    (if (occurs? x e)
	(case-record e
	  [(Var y)
	   (make-Var 'I)]
	  [(Lam y e)
	   (ba-var x (ba-var y e))]
	  [(App e0 e1)
	   (make-App (make-App (make-Var 'S)
			       (ba-var x e0))
		     (ba-var x e1))]
	  [else
	   (exit 'ba-var)])
	(make-App (make-Var 'K) (ba e)))))

(define not
  (lambda (x)
    (if x
	#f
	#t)))

(define occurs?
  (lambda (x e)
    (case-record e
      [(Var y)
       (eqv? x y)]
      [(Lam y e)
       (and (not (eqv? x y))
	    (occurs? x e))]
      [(App e0 e1)
       (or (occurs? x e0)
	   (occurs? x e1))]
      [else
       (exit 'occurs?)])))

;;;;;;;;;;

(define main
  (lambda (e)
    (unparse (ba (parse e)))))

;;;;;;;;;;

(list
  (main '(lambda (x) (lambda (y) y)))
  (main '(lambda (f) (lambda (g) (lambda (x) (f (g x))))))
  (main '(lambda (f) (lambda (x) (lambda (y) ((f y) x))))))

;;; expected result:
;;; ((K I)
;;;  ((S ((S (K S)) ((S (K K)) ((S (K S)) ((S (K K)) I)))))
;;;   (K ((S ((S (K S)) ((S (K K)) I))) (K I))))
;;;  ((S ((S (K S))
;;;       ((S (K K))
;;;        ((S (K S)) ((S ((S (K S)) ((S (K K)) I))) (K I))))))
;;;   (K ((S (K K)) I))))
