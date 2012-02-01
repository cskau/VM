;;; lib.DAIMI-Scheme/associate.scm
;;; assq and assoc

;;; requires "equal.scm"

;;;;;;;;;;

(define associate
  (lambda (p)
    (lambda (key a-list)
      (letrec ([loop
		(lambda (a-list)
		  (if (null? a-list)
		      #f
		      (let ([pair (car a-list)])
			(if (p key (car pair))
			    pair
			    (loop (cdr a-list))))))])
	(loop a-list)))))

(define assq (associate eqv?))

(define assoc (associate equal?))

;;;;;;;;;;

;;; end of "associate.scm"
