;;; lib.DAIMI-Scheme/curry-uncurry.scm
;;; 


;;;;;;;;;;

(define curry
  (lambda (p)
    ;;; (A * B -> C) -> A -> B -> C
    (lambda (x)
      (lambda (y)
	(p x y)))))

(define uncurry
  (lambda (p)
    ;;; (A -> B -> C) -> A * B -> C
    (lambda (x y)
      ((p x) y))))

;;;;;;;;;;

(define curry3
  (lambda (p)
    ;;; (A1 * A2 * A3 -> B) -> A1 -> A2 -> A3 -> B
    (lambda (x)
      (lambda (y)
	(lambda (z)
	  (p x y))))))

(define uncurry3
  (lambda (p)
    ;;; (A1 -> A2 -> A3 -> B) -> A1 * A2 * A3 -> B
    (lambda (x y z)
      (((p x) y) z))))

;;;;;;;;;;

;;; end of "curry-uncurry.scm"
