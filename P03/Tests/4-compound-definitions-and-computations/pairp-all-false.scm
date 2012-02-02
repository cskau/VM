(define list
  (lambda xs
    xs))

(list (pair? 123)
      (pair? -123)
      (pair? "hello world")
      (pair? pair?)
      (pair? (lambda (x) x))
      (pair? list)
      (pair? #t)
      (pair? 'foo))

;;; expected result: (#f #f #f #f #f #f #f #f)

