(define improper_list
  (cons 5 (cons 10 17)))

(apply (lambda (x y) x) improper_list)
