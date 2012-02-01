;;; lib.DAIMI-Scheme/integer-string-conversions.scm

;;;;;;;;;;

(define integer->string
  (lambda (n)
    (apply string (integer->list-of-chars n))))

(define digit?
  (let ([zero (char->integer #\0)]
	[nine (char->integer #\9)])
    (lambda (c)
      (and (char? c)
	   (let ([i (char->integer c)])
	     (and (<= zero i) (<= i nine)))))))	  

(define digit->integer
  (let ([zero (char->integer #\0)])
    (lambda (d)
      (if (digit? d)
	  (- (char->integer d) zero)
	  (error 'digit->integer "not a digit: ~s" d)))))

(define integer->digit
  (let ([zero (char->integer #\0)])
    (lambda (i)
      (if (and (<= 0 i) (<= i 9))
	  (integer->char (+ i zero))
	  (error 'integer->digit "non-digitable integer: ~s" i)))))

(define string->integer
  (lambda (s)
    (if (string? s)
	(let ([len (string-length s)])
	  (if (or (= len 0)
		  (and (char=? (string-ref s 0) #\-)
		       (= len 1)))
	      (error 'string->integer "invalid string: ~s" s)
	      (letrec ([loop
			(lambda (offset a)
			  (if (= offset len)
			      a
			      (loop (+ offset 1)
				    (+ (digit->integer (string-ref s offset))
				       (* 10 a)))))])
		(if (char=? (string-ref s 0) #\-)
		    (- 0 (loop 1 0))
		    (loop 0 0)))))
	(error 'string->integer "not a string: ~s" s))))

;;;;;;;;;;

;;; end of "integer-string-conversions.scm"
