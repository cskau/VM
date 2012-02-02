;;; while-with-callcc.scm

;;; This example illustrates a case where a captured continuation is called
;;; more than once.


(define while
  (lambda (thunk-test thunk-body)
    (let ([jump (call/cc (lambda (k) k))])
      (if (thunk-test)
	  (begin
	    (thunk-body)
	    (jump jump))
	  'stop))))

(define fac
  (lambda (n)
    (let ([v n] [a 1])
      (begin
	(while (lambda ()
		 (> v 0))
	       (lambda ()
		 (begin
		   (set! a (* v a))
		   (set! v (- v 1)))))
	a))))

(fac 5)

;;; expected result: 120
