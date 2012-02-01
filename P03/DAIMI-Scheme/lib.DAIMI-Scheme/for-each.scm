;;; lib.DAIMI-Scheme/for-each.scm
;;; 

;;;;;;;;;;

(define for-each1
  (lambda (f l)
    ;;; (A -> B) * List(A) -> empty-list
    (letrec ([loop
	      (lambda (l)
		(if (null? l)
		    '()
		    (begin
		      (f (car l))
		      (loop (cdr l)))))])
      (loop l))))

;;;;;;;;;;

(define for-each
  (lambda xs
    ;;; [A1 * ... * An -> B] * List(A1) * ... * List(An) -> empty-list
    (if (< (length xs) 2)
	(error 'for-each "missing list(s)")
	(let ([f (car xs)]
	      [ls (cdr xs)])
	  (cond
	    [(null? (cdr ls))
	     (for-each1 f (car ls))]
	    [(all-same-length? ls)
	     (letrec ([loop
		       (lambda (ls)
			 (if (null? (car ls))
			     '()
			     (begin
			       (apply f (all-the-cars ls))
			       (loop (all-the-cdrs ls)))))])
	       (loop ls))]
	    [else
	     (error 'for-each "mismatching lists")])))))

;;;;;;;;;;

;;; end of "for-each.scm"
