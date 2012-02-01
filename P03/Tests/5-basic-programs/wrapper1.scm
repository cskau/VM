;;; wrapper1.scm

(define write-char
  (let ([original-write-char write-char])
    (lambda args
      (if (and (pair? args) (null? (cdr args)))
	  (original-write-char (car args) (current-output-port))
	  (apply original-write-char args)))))

(begin
  (write-char #\h)
  (write-char #\o)
  (write-char #\ )
  (write-char #\h (current-output-port))
  (write-char #\o (current-output-port))
  (exit 0))

;;; expected output: ho ho
