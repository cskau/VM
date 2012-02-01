;;; delete-one.scm

(load-relative "/users/courses/dOvs/Project03/DAIMI-Scheme/lib.DAIMI-Scheme/standard-prelude.scm")

(define delete-one
  (lambda (x ls)
    (filter-out (lambda (y)
		  (equal? x y))
		ls)))

(delete-one 3 '(0 1 2 3 4 5 6))

;;; expected result: (0 1 2 4 5 6)
