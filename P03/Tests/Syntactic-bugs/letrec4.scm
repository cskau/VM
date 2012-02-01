(letrec ([x (lambda (a) a)] [y (lambda (b) b)] [3 (lambda (c) c)])
  (x y))

;;; declared variables should be variables, not integers
