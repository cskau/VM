;;; tick-local.scm

(define make-counter
  (lambda (init)
    (let ([counter init])
      (lambda messages
	(case (car messages)
	  [(get)
	   counter]
	  [(up)
	   (set! counter (+ counter init))]
	  [(down)
	   (set! counter (- counter init))]
	  [(reset)
	   (set! counter init)]
	  [(set)
	   (set! counter (car (cdr messages)))]
	  [else
	   "Hvad sige du?"])))))

(define counter1 (make-counter 10))
(define counter2 (make-counter 20))

(begin
  (counter1 'up)
  (counter2 'down)
  (counter1 'reset)
  (counter1 'up)
  (counter1 'up)
  (counter2 'set 100)
  (+ (counter1 'get) (counter2 'get)))

;;; expected result: 130
