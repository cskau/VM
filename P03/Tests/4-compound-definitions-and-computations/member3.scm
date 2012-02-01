;;; member3.scm

;;;;;;;;;;

(define member
  (lambda (x xs)
    (cond
      [(null? xs)
       #f]
      [(eqv? x (car xs))
       #t]
      [else
       (member x (cdr xs))])))

(vector (member 'x '(a b c)) (member 'x '(a b x c)))

;;;;;;;;;;

;;; expected result: #2(#f #t)
