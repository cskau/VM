;;; echo.scm
;;; echoes all Scheme expressions until the eof object

(load-relative "/users/courses/dOvs/Project03/DAIMI-Scheme/lib.DAIMI-Scheme/standard-prelude.scm")

(define prompt-read
  (lambda (s)
    (begin
      (display s (current-output-port))
      (read))))

(define toplevel
  (lambda ()
    (let ([it (prompt-read "Go ahead, make my day: ")])
      (if (eof-object? it)
	  (begin
	    (newline (current-output-port))
	    "Do you feel lucky?")
	  (begin
	    (write it)
	    (newline (current-output-port))
	    (toplevel))))))

(toplevel)
