(define x 10)

(+ (let ([x 1])
     (begin
       (set! x 2)
       x))
   x)

;;; expected result: 12
