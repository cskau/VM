(+ 7 (call/cc 
       (lambda (k)
	 (quotient (k 10) 0))))

;;; expected result 17
