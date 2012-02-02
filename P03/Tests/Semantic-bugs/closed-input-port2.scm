
(define port
  (open-input-file "/users/courses/dOvs/Project03/Tests/testfiles/zz-testfile.txt"))

(define get-and-close
  (lambda (p)
    (begin
      (read-char p)
      (close-input-port p))))

(begin
  (read-char port)
  (get-and-close port)
  (read-char port))
