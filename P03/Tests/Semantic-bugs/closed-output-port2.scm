;;; For this output test, you should define your own file
;;; in your own space:

(define port
  (open-output-file "zz-testfile5"))

(define put-and-close
  (lambda (p)
    (begin
      (write-char #\* p)
      (close-output-port p))))

(begin
  (write-char #\a port)
  (put-and-close port)
  (write-char #\b port))

;;; write-char called on a closed output port

;;; NB: Remember to remove your own file
;;;     before running this test again.
