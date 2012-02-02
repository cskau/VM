;;; DAIMI-Scheme/lib/7dump.scm
;;; a dumper for compiled DAIMI-Scheme programs
;;; Olivier Danvy <danvy@brics.dk>
;;; October 2002, October 2003


;;;;;;;;;;
;;;

(define dump-compiled-program
  (lambda (p port)
    (case-record p
	[(Compiled-Program number-of-global-definitions
			   number-of-temporaries
			   number-of-results
			   the-lambdas
			   the-code
			   the-tag)
	 (begin
	   (display "(DAIMI-SchemeE03" port)
	   (newline port)
	   (display "  (" port)
	   (write number-of-global-definitions port)
	   (write-char char:space port)
	   (write number-of-temporaries port)
	   (write-char char:space port)
	   (write number-of-results port)
	   (write-char #\) port)
	   (newline port)
	   (if (null? the-lambdas)
	       (display "  ()" port)
	       (begin
		 (display "  (" port)
		 (write (car the-lambdas) port)
		 (for-each1 (lambda (a-lambda)
			      (begin
				(newline port)
				(display "   " port)
				(write a-lambda port)))
			    (cdr the-lambdas))
		 (display ")" port)))
	   (newline port)
	   (display "  (" port)
	   (write (car the-code) port)
	   (for-each1 (lambda (instruction)
			(begin
			  (newline port)
			  (if (and (eqv? (car instruction) 'label)
				   (char=?
				     (string-ref
				       (symbol->string (cadr instruction))
				       0)
				     #\l))
			      (newline port)
			      'ikke-noget)
			  (display "   " port)
			  (write instruction port)))
		      (cdr the-code))
	   (display ")" port)
	   (newline port)
	   (display "  " port)
	   (write the-tag port)
	   (display ")" port)
	   (newline port))]
	[else
	 (error 'dump-compiled-program
		"not a compiled program: ~s" p)])))

;;;;;;;;;;

;;; end of "7dump.scm"
