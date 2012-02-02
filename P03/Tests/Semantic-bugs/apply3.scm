(define list (lambda xs xs))

(apply (lambda (x) x) (list 2 3))

;;; arity mismatch
