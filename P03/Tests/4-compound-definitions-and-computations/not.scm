(define not
  (lambda (x)
    (if x
	#f
	#t)))

(not (not (not #t)))

;;; expected result: #f
