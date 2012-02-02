(define foo
  (lambda (x)
    (foo (string-append x x))))

(foo "=")

;;; string overflow, probably
