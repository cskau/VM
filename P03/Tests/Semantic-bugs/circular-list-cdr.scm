(define list
  (lambda xs
    xs))

(define y (list 1 2 3))

(begin
  (set-cdr! y y)
  y)

;;; expected result: a circular list of 1's, ie, something not fit for print.
