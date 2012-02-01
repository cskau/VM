(define foo
  (lambda (x bar)
    (+ ((lambda ()
	 (+
	   (if x
	       52
	       (bar))
	   0)))
       0)))

(vector (foo #t (lambda () 26)) (foo #f (lambda () 26)))

;;; expected result: #2(52 26)

