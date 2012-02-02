(define factorial
  (lambda (x)
    (if (= x 0)
	1
	(* x (factorial (- x 1))))))

(factorial 1000)

;;; integer overflow
