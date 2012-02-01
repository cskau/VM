;;; For this output test, you should define your own file
;;; in your own space:

(define my-own-file
  "zz-testfile")

(define my-own-output-port
  (open-output-file my-own-file))

(output-port? my-own-output-port)

;;; expected result #t

;;; NB: Remember to remove your own file
;;;     before running this test again.
