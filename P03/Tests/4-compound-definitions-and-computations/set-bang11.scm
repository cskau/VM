(let* ([x 1])
  (begin
    (set! x 2)
    x))

;;; expected result: 2
