(define improper_list
  (cons 5 (cons 10 17)))

(apply (lambda (x y z) x) improper_list)
