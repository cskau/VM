(let* ([y 1]
       [x y])
  (begin
    (set! y 3)
    (+ y x)))

;;; expected result: 4
