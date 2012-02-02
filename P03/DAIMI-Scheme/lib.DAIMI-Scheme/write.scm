;;; lib.DAIMI-Scheme/write.scm
;;; 

;;;;;;;;;;

(define write-to-port
  (lambda (value port)
    (for-each (lambda (c)
		(write-char c port))
	      (value->list-of-chars value))))

(define write
  (lambda args
    (case (length args)
      [(1)
       (write-to-port (car args) (current-output-port))]
      [(2)
       (write-to-port (car args) (cadr args))]
      [else
       (error 'write "wrong arity: ~s" args)])))

;;;;;;;;;;

;;; end of "write.scm"
