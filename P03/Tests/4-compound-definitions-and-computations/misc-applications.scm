(define list
  (lambda xs
    xs))

(list ((lambda (x) x) 1)
      ((lambda (x y z) y) 2 3 4)
      ((lambda (f x) (f x)) (lambda (z) z) 42))      

;;; expected result: (1 3 42)
