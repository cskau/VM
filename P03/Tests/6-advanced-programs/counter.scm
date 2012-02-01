;;; Counting in a local variable together with call/cc

(define theCont '())

(begin
    (let
        ([val 0])
     (let
	 ([t1 (call/cc (lambda (k) (set! theCont k)))]
	  [t2 val])
      (if (< t2 10)
        (begin
	  (write-char (integer->char (+ t2 48)) (current-output-port))
  	  (set! val (+ val 1)))
	(begin
	  (write-char #\newline (current-output-port))
	  (exit 0))
       )
      )
    )
  (theCont '())
)

;;; Expected output: 0123456789
