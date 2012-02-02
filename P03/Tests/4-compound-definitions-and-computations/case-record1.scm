(define-record (Cell something))
(define-record (Pair first second))

(define foo
  (lambda (x)
    (case-record x
      [(Cell a)
       a]
      [(Pair a b)
       (+ a b)]
      [else
       'woops])))

(+ (foo (make-Cell 10)) (foo (make-Pair 20 30)))

;;; expected result: 60
