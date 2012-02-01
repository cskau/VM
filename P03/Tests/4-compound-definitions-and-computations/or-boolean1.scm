(define not
  (lambda (x)
    (if x
	#f
	#t)))

(or (< 2 1)
    (<= 2 1)
    (not (<= 2 2))
    (not (= 2 2))
    (not (>= 2 2))
    (>= 2 3)
    (> 2 3))

;;; Expected result: #f
