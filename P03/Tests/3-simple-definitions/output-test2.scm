;;; For this output test, you should define your own file
;;; in your own space:

(define my-own-file
  "zz-testfile2")

(define my-output-port
  (open-output-file my-own-file))

(define nothing
  (write-char #\a my-output-port))

(define nothing
  (close-output-port my-output-port))

(define my-input-port
  (open-input-file my-own-file))

(define c (read-char my-input-port))

(define nothing
  (close-input-port my-input-port))

c

;;; expected result #\a

;;; NB: Remember to remove your own file
;;;     before running this test again.
