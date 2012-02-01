(letrec ([x (lambda (a) a)] [y (lambda (b) b)] [z 10])
  (x y))

;;; 10 should be a lambda-abstraction
