(define list
  (lambda xs
    xs))

(apply (lambda (x) x) (list 1 2))

;;; arity mismatch
