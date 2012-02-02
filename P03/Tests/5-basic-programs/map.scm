;;; map.scm

(load-relative "/users/courses/dOvs/Project03/DAIMI-Scheme/lib.DAIMI-Scheme/standard-prelude.scm")

(define add1 (lambda (x) (+ x 1)))

(list (map1 add1 '(0 1 2 3 4 5 6 7 8 9))
      (map add1 '(0 1 2 3 4 5 6 7 8 9))
      (map + '(1 2 3 4) '(4 3 2 1))
      (map (lambda (x y z)
	     (* x (* y z)))
	   '(1 2 3)
	   '(3 2 1)
	   '(10 10 10)))

;;; expected result: ((1 2 3 4 5 6 7 8 9 10)
;;;                   (1 2 3 4 5 6 7 8 9 10)
;;;                   (5 5 5 5)
;;;                   (30 40 30))
