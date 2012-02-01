(let ([y 1])
  ((lambda (x)
     (+ y x))
   (begin
     (set! y 3)
     y)))

;;; expected result: 6

;;; NB. The recommended style would be to factor out the side effects
;;;     as follows.
;;; 
;;; (let ([y 1])
;;;   (begin
;;;     (set! y 3)
;;;     ((lambda (x)
;;;        (+ y x))
;;;      y)))
