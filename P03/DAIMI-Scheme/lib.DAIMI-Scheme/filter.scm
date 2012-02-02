;;; lib.DAIMI-Scheme/filter.scm
;;; 

;;;;;;;;;;

(define filter-in++
  (lambda (p alpha l)
    ;;; [A -> Boolean] * [A -> B] * List(A) -> List(B)
    (letrec ([loop (lambda (l)
		     (cond
		       [(null? l)
			'()]
		       [(p (car l))
			(cons (alpha (car l)) (loop (cdr l)))]
		       [else
			(loop (cdr l))]))])
      (loop l))))

(define filter-in
  (lambda (p l)
    (filter-in++ p (lambda (x) x) l)))

(define filter-out++
  (lambda (p alpha l)
    ;;; [A -> Boolean] * [A -> B] * List(A) -> List(A)
    (filter-in++ (compose not p) alpha l)))

(define filter-out
  (lambda (p l)
    (filter-out++ p (lambda (x) x) l)))

;;;;;;;;;;

;;; end of "filter.scm"
