(define-record (Triple d n a))

(case-record (make-Triple 1 2 3)
  [(Triple x y z)
   (+ x y z)]
  [else
   'nothing
   'going])

;;; case-record's clauses have only one consequent
