(define error
  (lambda xs
    (exit xs)))

(define-record (Cell something))

(let ([c (make-Cell 42)])
  (case-record c
    [(Cell x)
     x]
    [else
     (error 'hovsa "Not a cell")]))

;;; expected answer: 42
