;;; lib.DAIMI-Scheme/all-the-cxr.scm
;;; 

;;; requires "map.scm"

;;;;;;;;;;

(define all-same-length?
  (lambda (ls)
    ;;; List(List(A)) --> Boolean
    (let ([len (length (car ls))])
      (andmap1 (lambda (l)
		 (= len (length l)))
	       (cdr ls)))))

(define all-the-cars
  (lambda (ls)
    ;;; List(List(A)) --> List(A)
    (map1 car ls)))

(define all-the-cdrs
  (lambda (ls)
    ;;; List(List(A)) --> List(List(A))
    (map1 cdr ls)))

;;;;;;;;;;

;;; end of "all-the-cxr.scm"
