;;; This program outputs the source definition of the identity function, in Scheme.

(define list
  (lambda xs
    xs))

(define make-identity
  (lambda (x)
    (list 'lambda (list x) x)))

(make-identity 'a)

;;; expected result: (lambda (a) a)
