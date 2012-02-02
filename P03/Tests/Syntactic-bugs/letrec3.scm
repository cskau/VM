(letrec ([x (lambda (a) a)] [y (lambda (b) b)] [case (lambda (c) c)])
  (x y))

;;; declared variables should be variables, not keywords
