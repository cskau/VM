;;; nat-stream-call-by-need.scm

;;;;;;;;;;

(define force
  (lambda (s)
    (s)))

(define make-stream
  (lambda (seed next)
    (letrec ([loop (lambda (seed)
		     (cons seed (delay (loop (next seed)))))])
      (loop seed))))

(define stream-car
  car)

(define stream-cdr
  (lambda (s)
    (force (cdr s))))

;;;;;;;;;;

(define even-nats	; the stream of even natural numbers
  (make-stream 0 (lambda (n)
		   (+ 2 n))))

(define odd-nats	; the stream of odd natural numbers
  (make-stream 1 (lambda (n)
		   (+ 2 n))))

(define merge-streams
  (lambda (s1 s2 predicate)
    (if (predicate (stream-car s1) (stream-car s2))
	(cons (stream-car s1)
	      (delay (merge-streams (stream-cdr s1) s2 predicate)))
	(cons (stream-car s2)
	      (delay (merge-streams s1 (stream-cdr s2) predicate))))))

(define prefix-stream
  (lambda (s n)
    (if (= n 0)
	'()
	(cons (stream-car s)
	      (prefix-stream (stream-cdr s) (- n 1))))))


;;; The 10 first elements of merging even and odd numbers orderly:

(prefix-stream (merge-streams even-nats odd-nats <) 10)

;;; expected result: the list (0 1 2 3 4 5 6 7 8 9)
