
(define port
  (open-input-file "/users/courses/dOvs/Project03/Tests/testfiles/zz-testfile.txt"))

(begin
  (read-char port)
  (close-input-port port)
  (read-char port))
