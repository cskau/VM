(define foo
  (let ([a 0] [b 0] [c 0] [x 0] [y 0] [z 0])
    (lambda (y)
      (let ([bar '()] [quux x])
	(let ([a a] [b b] [c c] [z x] [y y])
	  (let ([baz '()] [frotz bar])
	    (begin
	      (set! x y)
	      z)))))))

(let* ([x0 (foo 1)]
       [x1 (foo 2)]
       [x2 (foo 3)]
       [x3 (foo 4)]
       [x4 (foo 8)]
       [x8 (foo 9)]
       [x9 (foo 1000)])
  (vector x0 x1 x2 x3 x4 x8 x9))

;;; expected result: #7(0 1 2 3 4 8 9)
