(letrec ([x (lambda (a) a)] [y (lambda (b) b)) [z (lambda (c) c)])
  (x y))

;;; mismatching parentheses
