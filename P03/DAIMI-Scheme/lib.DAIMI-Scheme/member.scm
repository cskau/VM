;;; lib.DAIMI-Scheme/member.scm
;;; 

;;;;;;;;;;

(define memq
  (lambda (e es)
    (letrec ([loop
	      (lambda (es)
		(and (not (null? es))
		     (if (eqv? e (car es))
			 es
			 (loop (cdr es)))))])
      (loop es))))

(define memq?
  (lambda (e es)
    (pair? (memq e es))))

;;;;;;;;;;

(define member
  (lambda (e es)
    (letrec ([loop
	      (lambda (es)
		(and (not (null? es))
		     (if (equal? e (car es))
			 es
			 (loop (cdr es)))))])
      (loop es))))

(define member?
  (lambda (e es)
    (pair? (member e es))))

;;;;;;;;;;

;;; end of "member.scm"
