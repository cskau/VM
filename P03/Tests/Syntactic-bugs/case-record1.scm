(define-record (Triple d n a))

(case-record (make-Triple 1 2 3)
  [(Triple x y z)
   (+ x y z)])

;;; case-record's last clause must be an else clause
