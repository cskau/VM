;;; delete-many.scm

(load-relative "/users/courses/dOvs/Project03/DAIMI-Scheme/lib.DAIMI-Scheme/standard-prelude.scm")

(define delete-many
  (lambda (xs ls)
    (filter-out (lambda (x)
		  (member? x xs))
		ls)))

(delete-many '(1 3 5) '(0 1 2 3 4 5 6))

;;; expected result: (0 2 4 6)
