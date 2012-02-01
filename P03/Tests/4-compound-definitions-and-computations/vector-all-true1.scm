(vector (integer? 123)
	(integer? -123)
	(integer? (string-length "12345"))
	(integer? (vector-length (make-vector 10 0))))

;;; expected result: #4(#t)
