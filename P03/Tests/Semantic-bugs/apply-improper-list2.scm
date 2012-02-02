(define improper_list
  (cons 5 (cons 10 #f)))

(apply (lambda (x y) x) improper_list)
