;;; lib.DAIMI-Scheme/fold.scm

;;;;;;;;;;

(define foldr
  (lambda (f b l)
    ;;; [A * B -> B] * B * List(A) -> B
    (letrec ([loop
	      (lambda (l)
		(if (null? l)
		    b
		    (f (car l) (loop (cdr l)))))])
      (loop l))))

(define foldl
  (lambda (f b l)
    ;;; [A * B -> C] * B * List(A) -> C
    (letrec ([loop (lambda (l b)
		     (if (null? l)
			 b
			 (loop (cdr l) (f (car l) b))))])
      (loop l b))))

;;;;;;;;;;

(define right-fold1
  (lambda (f b l)
    ;;; [B * A -> B] * B * List(A) -> B
    (letrec ([fast-loop
	      (lambda (l)
		(if (null? l)
		    b
		    (f (fast-loop (cdr l)) (car l))))])
      (fast-loop l))))

(define left-fold1
  (lambda (f b l)
    ;;; [B * A -> C] * B * List(A) -> C
    (letrec ([loop (lambda (l b)
		     (if (null? l)
			 b
			 (loop (cdr l) (f b (car l)))))])
      (loop l b))))

(define variadic-right-fold
  (lambda xs
    ;;; [B * A1 * ... * An -> B] * B * List(A1) * ... * List(An) -> B
    (if (<= (length xs) 2)
	(error 'right-fold "missing list(s)")
	(let ([f (car xs)]
	      [b (cadr xs)]
	      [ls (cddr xs)])
	  (cond
	    [(null? (cdr ls))
	     (right-fold1 f b (car ls))]
	    [(all-same-length? ls)
	     (letrec ([loop
		       (lambda (ls)
			 (if (null? (car ls))
			     b
			     (apply f
				    (cons (loop (all-the-cdrs ls))
					  (all-the-cars ls)))))])
	       (loop ls))]
	    [else
	     (error 'variadic-right-fold "mismatching lists")])))))

(define variadic-left-fold
  (lambda xs
    ;;; [B * A1 * ... * An -> C] * B * List(A1) * ... * List(An) -> C
    (if (<= (length xs) 2)
	(error 'left-fold "missing list(s)")
	(let ([f (car xs)]
	      [b (cadr xs)]
	      [ls (cddr xs)])
	  (cond
	    [(null? (cdr ls))
	     (left-fold1 f b (car ls))]
	    [(all-same-length? ls)
	     (letrec ([loop
		       (lambda (ls b)
			 (if (null? (car ls))
			     b
			     (loop (all-the-cdrs ls)
				   (apply f (cons b (all-the-cars ls))))))])
	       (loop ls b))]
	    [else
	     (error 'variadic-left-fold "mismatching lists")])))))

;;;;;;;;;;

;;; end of "fold.scm"
