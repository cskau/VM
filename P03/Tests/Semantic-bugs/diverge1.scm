(define foo
  (lambda (x)
    (foo (cons 1 x))))

(foo '())

;;; this program diverges, and executing it should run out of memory
