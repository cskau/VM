;;; lib.DAIMI-Scheme/display.scm
;;; 

;;;;;;;;;;

(define display-to-port
  (lambda (value port)
    (for-each (lambda (c)
		(if (char=? c #\")
		    '()
		    (write-char c port)))
	      (value->list-of-chars value))))

(define display
  (lambda args
    (case (length args)
      [(1)
       (display-to-port (car args) (current-output-port))]
      [(2)
       (display-to-port (car args) (cadr args))]
      [else
       (error 'display "wrong arity: ~s" args)])))

;;;;;;;;;;

;;; end of "display.scm"
