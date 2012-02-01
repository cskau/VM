(define list
  (lambda xs
    xs))

(list (pair? '(a b))
      (pair? (cons 'a (cons 'b '()))))

;;; expected result: (#t #t)

