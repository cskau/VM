(define foo
  (lambda ()
    (null? (foo))))

(foo)

;;; this program diverges, and executing it should run out of memory
