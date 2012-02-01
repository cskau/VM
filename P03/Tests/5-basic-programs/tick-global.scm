;;; tick-global.scm

(define counter 0)

(define tick!
  (lambda ()
    (set! counter (+ counter 1))))

(define reset-counter!
  (lambda ()
    (set! counter 0)))


(begin
  (tick!)
  (reset-counter!)
  (tick!)
  (tick!)
  counter)

;;; expected result: 2
