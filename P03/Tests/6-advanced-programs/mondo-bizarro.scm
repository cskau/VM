;;; mondo-bizarro.scm

;;; People who like this kind of things should certainly find that this
;;; is the kind of things they like.  What does the following program do?
;;; Be a sport and try to figure it out by hand first.

(define mondo-bizarro
  (lambda ()
    (let ([k (call/cc (lambda (c) c))])
      (begin
	(write-char #\a (current-output-port))
	(call/cc (lambda (c) (k c)))
	(write-char #\b (current-output-port))
	(call/cc (lambda (c) (k c)))
	(write-char #\c (current-output-port))
	(write-char #\newline (current-output-port))))))

(begin
  (mondo-bizarro)
  (exit 0))
