;;; enter-once-and-exit-twice.scm

;;; This example illustrates a case where a captured continuation is
;;; -restored- more than once.

(define-record (Stuff flag continuation))

(define newline
  (lambda (port)
    (write-char #\newline port)))

(define foo
  (lambda (escape)
    (begin
      (write-char #\H (current-output-port))
      (write-char #\e (current-output-port))
      (write-char #\l (current-output-port))
      (write-char #\l (current-output-port))
      (write-char #\o (current-output-port))
      (newline (current-output-port))
      (case-record (call/cc escape)
	[(Stuff flag continuation)
	 (begin
	   (if flag
	     (begin
	       (write-char #\w (current-output-port))
	       (write-char #\o (current-output-port))
	       (write-char #\r (current-output-port))
	       (write-char #\l (current-output-port))
	       (write-char #\d (current-output-port)))
	     (begin
	       (write-char #\A (current-output-port))
	       (write-char #\a (current-output-port))
	       (write-char #\r (current-output-port))
	       (write-char #\h (current-output-port))
	       (write-char #\u (current-output-port))
	       (write-char #\s (current-output-port))))
	   (newline (current-output-port))
	   (continuation 'done))]
	[else
	 (exit 1)]))))


(let ([k (call/cc foo)])
  (begin
    (write-char #\1 (current-output-port))
    (newline (current-output-port))
    (call/cc (lambda (a)
	       (k (make-Stuff #t a))))
    (write-char #\2 (current-output-port))
    (newline (current-output-port))
    (call/cc (lambda (b)
	       (k (make-Stuff #f b))))
    (write-char #\3 (current-output-port))
    (newline (current-output-port))
    'ok?))

;;; expected output:
;;; Hello
;;; 1
;;; world
;;; 2
;;; Aarhus
;;; 3
;;; ok?
