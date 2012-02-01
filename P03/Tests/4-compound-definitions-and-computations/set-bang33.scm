(let* ([a 1])
  (let* ([b 2])
    (let* ([c 3])
      (let* ([d 4])
	(let* ([e 5])
	  (begin
	    (set! b 20)
	    (set! d 40)
	    (vector a b c d e)))))))

;;; expected result: #5(1 20 3 40 5)
