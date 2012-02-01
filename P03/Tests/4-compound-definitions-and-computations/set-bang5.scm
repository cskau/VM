(define foo
  (let ([x 0])
    (lambda (y)
      (let ([z x])
	(begin
	  (set! x y)
	  z)))))

(let* ([x0 (foo 1)]
       [x1 (foo 2)]
       [x2 (foo 3)]
       [x3 (foo 4)]
       [x4 (foo 8)]
       [x8 (foo 9)]
       [x9 (foo 1000)])
  (vector x0 x1 x2 x3 x4 x8 x9))

;;; expected result: #7(0 1 2 3 4 8 9)
