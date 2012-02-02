;;; DAIMI-Scheme.compiler/lib/verbose.scm
;;; 
;;; Olivier Danvy <danvy@brics.dk>
;;; October 2002

(define warn
  (lambda (msg)
    (if verbose
	(display msg (current-output-port))
	"Shhh!")))

(define warnl
  (lambda (msg)
    (if verbose
	(begin
	  (display msg (current-output-port))
	  (newline (current-output-port)))
	"Shhh!")))

;;; end of "verbose.scm"
