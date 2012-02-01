(define-record (Triple d n a))

(case-record (make-Triple 1 2 3)
  [(Triple x y x)
   (+ x y x)]
  [else
   'nothing])

;;; the variables declared in each clause must be distinct
