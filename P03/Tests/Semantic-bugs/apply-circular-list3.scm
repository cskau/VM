(define circular-list
  (let ([x (cons 'foo (cons 'bar '()))])
    (begin
      (set-cdr! (cdr x) (cdr x))
      x)))

(apply (lambda (x) x) circular-list)

;;; circular list
