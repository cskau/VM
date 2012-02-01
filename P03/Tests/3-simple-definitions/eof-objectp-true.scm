(define port
  (open-input-file "/users/courses/dOvs/Project03/Tests/testfiles/zz-emptyfile"))

(define c
  (peek-char port))

(eof-object? c)

;;; expected result #t
