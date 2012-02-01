(define x 1)
(define foo
  (lambda (y1 y2)
    (lambda (z)
      (begin
	(set! x z)
	(set! y1 z)
	(set! y2 z)
	(+ (+ y1 y2) z)))))
((foo 10 20) 30)

;;; expected result: 90
