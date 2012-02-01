(let ([a 1] [b 2] [c 3] [d 4] [e 5])
  (begin
    (set! b 20)
    (set! d 40)
    (vector a b c d e)))

;;; expected result: #5(1 20 3 40 5)
