(define port
  (open-input-file "/users/courses/dOvs/Project03/Tests/testfiles/zz-testfile.txt"))

(define c1
  (read-char port))

(define c2
  (read-char port))

(define nothing
  (close-input-port port))

c2

;;; expected result #\h
