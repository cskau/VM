;;; lib.DAIMI-Scheme/cxxr.scm
;;; 

;;; requires "compose.scm"

;;;;;;;;;;

(define caar
  ;;; (compose car car)
  (lambda (p)
    (car (car p))))

(define cadr
  ;;; (compose car cdr)
  (lambda (p)
    (car (cdr p))))

(define cdar
  ;;; (compose cdr car)
  (lambda (p)
    (cdr (car p))))

(define cddr
  ;;; (compose cdr cdr)
  (lambda (p)
    (cdr (cdr p))))

;;;;;;;;;;

;;; end of "cxxr.scm"
