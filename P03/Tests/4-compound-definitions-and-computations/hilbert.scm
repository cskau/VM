(define I
  (lambda (x)
    x))

(define K
  (lambda (x)
    (lambda (y)
      x)))

(((K I) 'foo) 42)

;;; expected answer: 42
