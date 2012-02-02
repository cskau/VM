(+ 7 (call/cc 
       (lambda (k)
	 (k 10))))

;;; expected result 17
