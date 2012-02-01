;;; lib.DAIMI-Scheme/map.scm
;;; 

;;;;;;;;;;

(define map1
  (lambda (f l)
    ;;; [A -> B] * List(A) -> List(B)
    (letrec ([loop
	      (lambda (l)
		(if (null? l)
		    '()
		    (cons (f (car l)) (loop (cdr l)))))])
      (loop l))))

(define map-append1
  (lambda (f l)
    ;;; [A -> List(B)] * List(A) -> List(B)
    (letrec ([loop
	      (lambda (l)
		(if (null? l)
		    '()
		    (append (f (car l)) (loop (cdr l)))))])
      (loop l))))

(define andmap1
  (lambda (f l)
    ;;; [A -> Bool] * List(A) -> Bool
    (letrec ([loop
	      (lambda (l)
		(if (null? l)
		    #t
		    (and (f (car l))
			 (loop (cdr l)))))])
      (loop l))))

(define ormap1
  (lambda (f l)
    ;;; [A -> Bool] * List(A) -> Bool
    (letrec ([loop
	      (lambda (l)
		(if (null? l)
		    #f
		    (or (f (car l))
			(loop (cdr l)))))])
      (loop l))))

;;;;;;;;;;

(define map
  (lambda xs
    ;;; [A1 * ... * An -> B] * List(A1) * ... * List(An) -> List(B)
    (if (< (length xs) 2)
	(error 'map "missing list(s)")
	(let ([f (car xs)]
	      [ls (cdr xs)])
	  (cond
	    [(null? (cdr ls))
	     (map1 f (car ls))]
	    [(all-same-length? ls)
	     (letrec ([loop
		       (lambda (ls)
			 (if (null? (car ls))
			     '()
			     (cons (apply f (all-the-cars ls))
				   (loop (all-the-cdrs ls)))))])
	       (loop ls))]
	    [else
	     (error 'map "mismatching lists")])))))

(define map-append
  (lambda xs
    ;;; [A1 * ... * An -> List(B)] * List(A1) * ... * List(An) -> List(B)
    (if (< (length xs) 2)
	(error 'map-append "missing list(s)")
	(let ([f (car xs)]
	      [ls (cdr xs)])
	  (cond
	    [(null? (cdr ls))
	     (map-append1 f (car ls))]
	    [(all-same-length? ls)
	     (letrec ([loop
		       (lambda (ls)
			 (if (null? (car ls))
			     '()
			     (append (apply f (all-the-cars ls))
				     (loop (all-the-cdrs ls)))))])
	       (loop ls))]
	    [else
	     (error 'map-append "mismatching lists")])))))

;;;;;;;;;;

;;; end of "map.scm"
