;;; DAIMI-Scheme/lib/standard-prelude.scm
;;; Petite Chez Scheme standard library for running DAIMI-Scheme programs
;;; Olivier Danvy <danvy@brics.dk>
;;; October 2002, October 2003


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; records facilities:

(load "lib/records.scm")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; standard boolean utilities:

;;; ikke noget


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; standard procedure stuff:

(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

(define compose2
  (lambda (f g)
    (lambda (x y)
      (f (g x y)))))

(define composeN
  (lambda (f g)
    (lambda xs
      (f (apply g xs)))))

(define curry
  (lambda (f)
    (lambda (x)
      (lambda (y)
	(f x y)))))

(define uncurry
  (lambda (f)
    (lambda (x y)
      ((f x) y))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  standard equality:

;;; ikke noget


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; standard list utilities:

(define proper-list?
  (lambda (v)
    (or (null? v)
	(and (pair? v)
	     (proper-list? (cdr v))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  standard S-expression-processing stuff:

(define snoc
  (lambda (d a)
    (cons a d)))

(define caddddr (compose car cddddr))

(define ref-list
  (lambda (xs x)
    (letrec ([loop
	      (lambda (ys offset)
		(cond
		  [(null? ys)
		   (error 'ref-list "not in the list: ~s ~s" xs x)]
		  [(equal? (car ys) x)
		   offset]
		  [else
		   (loop (cdr ys) (1+ offset))]))])
      (loop xs 0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  standard list-processing utilities:

(define for-each1 for-each)

(define map1	;;; with left-to-right evaluation
  (lambda (f xs)
    (letrec ([walk
	      (lambda (xs)
		(if (null? xs)
		    '()
		    (let ([y (f (car xs))])
		      (cons y (walk (cdr xs))))))])
      (walk xs))))

(define map-append1
  (lambda (f l)
    ;;; (A -> List(B)) * List(A) -> List(B)
    (apply append (map1 f l))))

(define andmap1 andmap)

(define ormap1 ormap)

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

(define filter-in
  (lambda (p l)
    ;;; [A -> Boolean] * List(A) -> List(A)
    (letrec ([loop (lambda (l)
		     (cond
		       [(null? l)
			'()]
		       [(p (car l))
			(cons (car l) (loop (cdr l)))]
		       [else
			(loop (cdr l))]))])
      (loop l))))

(define filter-out
  (lambda (p l)
    ;;; [A -> Boolean] * List(A) -> List(A)
    (filter-in (compose not p) l)))

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

(define all-same-length?
  (lambda (ls)
    ;;; List(List(A)) --> Boolean
    (let ([len (length (car ls))])
      (andmap1 (lambda (l)
		 (= len (length l)))
	       (cdr ls)))))

(define all-the-cars
  (lambda (ls)
    ;;; List(List(A)) --> List(A)
    (map1 car ls)))

(define all-the-cdrs
  (lambda (ls)
    ;;; List(List(A)) --> List(List(A))
    (map1 cdr ls)))

(define map-append
  (lambda xs
    ;;; [A1 * ... * An -> List(B)] * List(A1) * ... * List(An) -> List(B)
    (if (< (length xs) 2)
	(error 'map-append "missing list(s)")
	(apply append (apply map (car xs) (cdr xs))))))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  integer utilities:

;;; ikke noget


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  char utilities:

(define char:space #\space)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; string utilities:

(define string-suffix?
  (lambda (text suffix)
    (cond
      [(not (string? text))
       (error 'string-suffix? "not a string: ~s" text)]
      [(not (string? suffix))
       (error 'string-suffix? "not a string: ~s" suffix)]
      [else
       (let ([text-len (string-length text)]
	     [suffix-len (string-length suffix)])
	 (and (>= text-len suffix-len)
	      (string=? (substring text
				   (- text-len suffix-len)
				   text-len)
			suffix)))])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vector utilities:

;;; ikke noget


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  conversion routines

(define integer->string number->string)

(define string->integer string->number)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; i/o utilities:

(define dump-compiled-program
  (lambda (p port)
    (case-record p
	[(Compiled-Program number-of-global-definitions
			   number-of-temporaries
			   number-of-results
			   the-lambdas
			   the-code
			   the-tag)
	 (pretty-print (list 'DAIMI-SchemeE03
			     number-of-global-definitions
			     number-of-temporaries
			     number-of-results
			     the-lambdas
			     the-code
			     the-tag)
		       port)]
	[else
	 (error dump-compiled-program
		"not a compiled program: ~p" p)])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; misc.

;;; ikke noget


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 

;;; end of "standard-prelude.scm"
