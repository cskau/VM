;;; coroutines.scm
;;; Gerth Brodal & Olivier Danvy, fall 1993
;;; updated October 1994 and November 2002


(load-relative "/users/courses/dOvs/Project03/DAIMI-Scheme/lib.DAIMI-Scheme/standard-prelude.scm") 

; ---- 

(define make-queue
  (lambda ()
    (let ([hook (cons '() '())])
      (cons hook hook))))

(define empty-queue?
  (lambda (q)
    (eqv? (car q) (cdr q))))

(define enqueue!
  (lambda (q a)
    (begin
      (set-cdr! (cdr q) (cons a '()))
      (set-cdr! q (cdr (cdr q))))))

(define dequeue!
  (lambda (q)
    (if (empty-queue? q)
	(error 'dequeue! "can't dequeue an empty queue")
	(begin
	  (set-car! q (cdr (car q)))
	  (car (car q))))))

; ----

;(define force
;  (lambda (thunk)
;    (thunk)))

; ----

(define sleeping-queue
  (make-queue))

(define pop-process!
  (lambda ()
    (if (empty-queue? sleeping-queue)
	(lambda (dummy)
	  (begin
	    (newline (current-output-port))
	    (exit 0)))
	(dequeue! sleeping-queue))))

(define push-process!
  (lambda (p)
    (enqueue! sleeping-queue p)))

(define start! push-process!)

(define suspend!
  (lambda ()
     (call/cc (lambda (k)
	        (begin 
	          (push-process! k)
	          (run-processes!))))))

(define run-processes!
  (lambda ()
    ((pop-process!) 'go)))

(define terminate! run-processes!)

; ---- 

(define loop
  (lambda (thunk counter)
    (letrec ([loop (lambda (counter)
		     (if (= counter 0)
			 (terminate!)
			 (begin
			   (thunk)
			   (loop (- counter 1)))))])
      (loop counter))))

(define process-A
  (lambda (dummy)
    (loop (lambda ()
	    (begin 
	      (write-char #\A (current-output-port))
	      (suspend!)))
	  15)))

(define process-B
  (lambda (dummy)
    (loop (lambda ()
	    (begin 
	      (write-char #\B (current-output-port))
	      (suspend!)))
	  5)))

(define process-C
  (lambda (dummy)
    (loop (lambda ()
            (begin 
	      (write-char #\C (current-output-port))
	      (suspend!)))
	  10)))

(define test
  (lambda ()
    (begin
      (start! process-A)
      (start! process-B)
      (start! process-C)
      (run-processes!))))

(test)

; ----

;;; expected output: ABCABCABCABCABCACACACACACAAAAA
