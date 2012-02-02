;;; lib.DAIMI-Scheme/vector.scm
;;; 

;;;;;;;;;;

(define vector->list
  (lambda (v)
    (let ([len (vector-length v)])
      (letrec ([loop
		(lambda (i)
		  (if (= i len)
		      '()
		      (cons (vector-ref v i)
			    (loop (+ i 1)))))])
	(loop 0)))))

(define list->vector
  (lambda (l)
    (let* ([len (length l)]
	   [v (make-vector len '())])
      (letrec ([loop
		(lambda (i l)
		  (if (= i len)
		      v
		      (begin
			(vector-set! v i (car l))
			(loop (+ i 1) (cdr l)))))])
	(loop 0 l)))))

;;;;;;;;;;

;;; end of "vector.scm"
