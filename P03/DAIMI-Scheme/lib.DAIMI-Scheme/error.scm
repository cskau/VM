;;; lib.DAIMI-Scheme/error.scm
;;; a Chez Scheme-like error procedure

;;; requires "display.scm"
;;; requires "format.scm"

;;;;;;;;;;

(define error
  (lambda msg
    (begin
      (set! error
	    (lambda msg
	      (begin
		(newline (current-output-port))
		(display "DAIMI-Scheme: error in error." (current-output-port))
		(newline (current-output-port))
		(exit 3))))
      (newline (current-output-port))
      (display "DAIMI-Scheme error -- " (current-output-port))
      (if (null? msg)
	    (begin
	      (newline (current-output-port))
	      (exit 1))
	    (let* ([name (car msg)]
		   [printable-name (if (symbol? name)
				       (symbol->string name)
				       name)])
	      (begin
		(display printable-name (current-output-port))
		(display " -- " (current-output-port))
		(display (apply format (cdr msg)) (current-output-port))
		(newline (current-output-port))
		(exit 2)))))))

;;;;;;;;;;

;;; end of "error.scm"
