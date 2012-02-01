(define list
  (lambda xs
    xs))

(define right-fold1
  (lambda (f b l)
    ;;; (B * A -> B) * B * List(A) -> B
    (letrec ([fast-loop
	      (lambda (l)
		(if (null? l)
		    b
		    (f (fast-loop (cdr l)) (car l))))])
      (fast-loop l))))

(define left-fold1
  (lambda (f b l)
    ;;; (B * A -> C) * B * List(A) -> C
    (letrec ([loop (lambda (l b)
		     (if (null? l)
			 b
			 (loop (cdr l) (f b (car l)))))])
      (loop l b))))

(define snoc
  (lambda (d a)
    (cons a d)))

(define concat
  (lambda (xs ys)
    ;;; List(A) * List(A) -> List(A)
    (right-fold1 snoc ys xs)))

(define append
  (lambda ls
    ;;; List(List(A)) -> List(A)
    (left-fold1 concat '() ls)))

(define reverse
  (lambda (l)
    ;;; List(A) -> List(A)
    (left-fold1 snoc '() l)))

(define length
  (lambda (xs)
    ;;; List(A) -> Integer
    (left-fold1 (lambda (a dummy)
		  (+ a 1))
		0
		xs)))

(list (concat '(1 2 3) '(4 5 6))
      (append '(1 2 3) '(4 5 6))
      (reverse '(1 2 3))
      (length '(1 2 3)))

;;; expected result: ((1 2 3 4 5 6) (1 2 3 4 5 6) (3 2 1) 3)
