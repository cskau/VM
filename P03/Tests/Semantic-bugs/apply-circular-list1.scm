(define circular-list
  (let ([x (cons 'foo '())])
    (begin
      (set-cdr! x x)
      x)))

(apply (lambda (x) x) circular-list)

;;; circular list
