;;; redefinition1.scm

(define foo
  (lambda ()
    (+ 2 4)))

(let* ([a (foo)]
       [c (begin
	    (set! + *)
	    (foo))])
  (+ a c))

;;; expected result: 48
