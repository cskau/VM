;;; lib.DAIMI-Scheme/integer.scm
;;; 

;;; requires "boolean.scm"

;;;;;;;;;;

(define 1+
  (lambda (n)
    (+ n 1)))

(define add1 1+)

(define 1-
  (lambda (n)
    (- n 1)))

(define sub1 1-)

(define negative?
  (lambda (n)
    (< n 0)))

(define zero?
  (lambda (n)
    (= n 0)))

(define positive?
  (lambda (n)
    (> n 0)))

(define even?
  (lambda (n)
    (= (remainder n 2) 0)))

(define odd?
  (lambda (n)
    (not (= (remainder n 2) 0))))

(define modulo
  (lambda (i j)
    (let ([r (remainder i j)])
      (if (negative? i)
	  (if (negative? j)
	      r
	      (+ r j))
	  (if (negative? j)
	      (+ r j)
	      r)))))

(define abs
  (lambda (n)
    (if (< n 0)
	(- 0 n)
	n)))

(define min
  (lambda (i j)
    (if (< i j)
	i
	j)))

(define max
  (lambda (i j)
    (if (< i j)
	j
	i)))

;;;;;;;;;;

;;; end of "integer.scm"
