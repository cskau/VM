;;; lib.DAIMI-Scheme/input-output.scm
;;; 

;;;;;;;;;;

(define call-with-input-file
  (lambda (filename procedure)
    (let* ([port (open-input-file filename)]
	   [result (procedure port)])
      (begin
	(close-input-port port)
	result))))

(define call-with-output-file
  (lambda (filename procedure)
    (let* ([port (open-output-file filename)]
	   [result (procedure port)])
      (begin
	(close-output-port port)
	result))))

(define newline
  (lambda xs
    (cond
      [(null? xs)
       (write-char #\newline (current-output-port))]
      [(null? (cdr xs))
       (let ([port (car xs)])
	 (if (output-port? port)
	     (write-char #\newline port)
	     (error 'newline "not a port: ~s" port)))]
      [else
       (error 'newline "Too many arguments: ~s" xs)])))
    

;;;;;;;;;;

;;; end of "input-output.scm"
