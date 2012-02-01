;;; lib.DAIMI-Scheme/char.scm
;;; char=?, char-whitespace?

;;;;;;;;;;

(define char=?
  (lambda (c1 c2)
    (= (char->integer c1) (char->integer c2))))

;;;;;;;;;;

(define char:tab (integer->char 9))		;;; #\tab
(define char:newline (integer->char 10))	;;; #\newline
(define char:space (integer->char 32))		;;; #\  aka #\space

(define char:whitespace?
  (lambda (c)
    (if (char? c)
	(let ([i (char->integer c)])
	  (if (= i 32)
	      #t
	      (if (= i 10)
		  #t
		  (= i 9))))
	(error 'char:whitespace? "not a char: ~s" c))))

(define char:separator?
  (lambda (c)
    (if (char:whitespace? c)
	#t
	(case c
	  [(#\( #\) #\[ #\] #\' #\` #\, #\;)
	   #t]
	  [else
	   #f]))))

;;;;;;;;;;

;;; end of "char.scm"
