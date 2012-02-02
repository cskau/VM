;;; echo.scm
;;; reads a Scheme file and returns its contents in a list

(load-relative "../lib.DAIMI-Scheme/standard-prelude.scm")

(define echo
  (lambda (filename)
    (call-with-input-file
      filename
      (lambda (port)
	(letrec ([walk
		  (lambda ()
		    (let ([r (read port)])
		      (if (eof-object? r)
			  '()
			  (cons r (walk)))))])
	  (walk))))))

(echo "tests/echo.scm")
;(echo "lib.DAIMI-Scheme/read.scm")

