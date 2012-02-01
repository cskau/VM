(define foo
  (lambda (x)
    (foo x)))

(foo 0)

;;; this program loops, but executing it should not run out of memory
