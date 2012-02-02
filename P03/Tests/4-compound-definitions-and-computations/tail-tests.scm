(define test
  (lambda (b1 b2)
    (+ ((lambda () (if b1 1 2))) ((lambda () (if b2 10 20))))))

(define list
  (lambda xs
    xs))

(list (test #f #f) (test #f #t) (test #t #f) (test #t #t))

;;; expected result: (22 12 21 11)
