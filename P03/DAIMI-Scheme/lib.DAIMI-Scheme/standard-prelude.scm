;;; lib.DAIMI-Scheme/standard-prelude.scm
;;; DAIMI-Scheme standard libraries


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; record facilities:

;;; ikke noget


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; standard boolean utilities:

(load-relative "boolean.scm")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; standard procedure stuff:

(load-relative "compose.scm")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  standard equality:

(load-relative "equal.scm")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; standard list-processing utilities:

(load-relative "list.scm")
(load-relative "member.scm")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  standard S-expression-processing stuff:

(load-relative "cxxr.scm")
(load-relative "cxxxr.scm")
(load-relative "cxxxxr.scm")
(load-relative "cxxxxxr.scm")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  standard list-processing utilities:

(load-relative "all-the-cxr.scm")
(load-relative "map.scm")
(load-relative "for-each.scm")
(load-relative "fold.scm")
(load-relative "filter.scm")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  integer utilities:

(load-relative "integer.scm")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; char utilities:

(load-relative "char.scm")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; string utilities:

(load-relative "string.scm")
(load-relative "format.scm")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vector utilities:

(load-relative "vector.scm")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  conversion routines

(load-relative "integer-string-conversions.scm")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; i/o

(load-relative "input-output.scm")
(load-relative "read.scm")
(load-relative "write.scm")
(load-relative "display.scm")

(define extract-directory-name
  (lambda (filename)
    (if (string? filename)
	(let ([len (string-length filename)])
	  (letrec ([walk (lambda (offset)
			   (cond
			     [(negative? offset)
			      ""]
			     [(char=? (string-ref filename offset) #\/)
			      (substring filename 0 (+ offset 1))]
			     [else
			      (walk (- offset 1))]))])
	    (walk (- len 1))))
	(error 'extract-directory-name "not a file name: ~s" filename))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; misc.

(load-relative "error.scm")
		
(load-relative "sort.scm")

(define call-with-current-continuation call/cc)

(define throw
  (lambda (continuation value)
    (continuation value)))

(define force
  (lambda (thunk)
    (thunk)))

(load-relative "associate.scm")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 

;;; end of "standard-prelude.scm"
