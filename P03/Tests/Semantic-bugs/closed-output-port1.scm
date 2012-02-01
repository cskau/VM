;;; For this output test, you should define your own file
;;; in your own space:

(define port
  (open-output-file "zz-testfile4"))

(begin
  (write-char #\a port)
  (close-output-port port)
  (write-char #\b port))

;;; write-char called on a closed output port

;;; NB: Remember to remove your own file
;;;     before running this test again.
