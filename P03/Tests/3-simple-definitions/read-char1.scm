(define port
  (open-input-file "/users/courses/dOvs/Project03/Tests/testfiles/zz-testfile.txt"))

(define c
  (read-char port))

(define nothing
  (close-input-port port))

c

;;; expected result: #\T
