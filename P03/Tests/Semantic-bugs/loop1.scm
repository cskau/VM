(define foo
  (lambda (x)
    (if x
	(foo x)
	'done)))

(foo 0)

;;; this program loops, but executing it should not run out of memory
