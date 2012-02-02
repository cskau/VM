(letrec ([x (lambda (a) a)] [y (lambda (b) b)] [x (lambda (c) c)])
  (x y))

;;; declared variables should all be distinct
