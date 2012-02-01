;;; lib.DAIMI-Scheme/read.scm
;;; A continuation-based read function for DAIMI-Scheme.

;;; Required:
;;; char->integer char:whitespace? reverse
;;; call/cc memq integer->char

;;;;;;;;;;

(define reader:read-char
  (lambda (port continue-with-char continue-with-eof-object)
    (let ([c (read-char port)])
      (if (eof-object? c)
	  (continue-with-eof-object c)
	  (continue-with-char c)))))

(define reader:peek-char
  (lambda (port continue-with-char continue-with-eof-object)
    (let ([c (peek-char port)])
      (if (eof-object? c)
	  (continue-with-eof-object c)
	  (continue-with-char c)))))

;;;;;;;;;;

;;; skips everything until the end of the current line
(define reader:skip-to-end-of-line
  (lambda (port continue continue-with-eof-object)
    (letrec ([loop (lambda ()
		     (reader:read-char
		       port
		       (lambda (c)
			 (if (char=? c char:newline)
			     (continue)
			     (loop)))
		       continue-with-eof-object))])
      (loop))))

;;; skips all the white space and peeks at the next character, if there is any
(define reader:skip-whitespace-peek
  (lambda (port continue-with-char continue-with-eof-object)
    (letrec ([loop
	      (lambda ()
		(reader:peek-char
		  port
		  (lambda (c)
		    (cond
		      [(char:whitespace? c)
		       (begin
			 (read-char port)
			 (loop))]
		      [(char=? c #\;)
		       (reader:skip-to-end-of-line
			 port
			 loop
			 continue-with-eof-object)]
		      [else
		       (continue-with-char c)]))
		  continue-with-eof-object))])
      (loop))))

;;; skips all the white space and reads the next character, if there is any
(define reader:skip-whitespace
  (lambda (port continue-with-char continue-with-eof-object)
    (reader:skip-whitespace-peek
      port
      (lambda (c)
	(begin
	  (read-char port)
	  (continue-with-char c)))
      continue-with-eof-object)))

;;; reads a decimal integer (but might return a symbol)
;;; total is the value of the number seen so far
(define reader:read-num
  (lambda (total port continue-with-token)
    (reader:peek-char
      port
      (lambda (c)
	(cond
	  [(digit? c)
	   (begin 
	     (read-char port)
	     (reader:read-num (+ (* 10 total) (digit->integer c))
			      port
			      continue-with-token))]
	  [(char:separator? c)
	   (continue-with-token total)]
	  [else
	   (reader:read-identifier
	     (read-char port)
	     port
	     (lambda (ident)
	       (continue-with-token
		 (string->symbol
		   (string-append
		     (integer->string total)
		     (symbol->string ident))))))]))
      (lambda (eof-object)
	(continue-with-token total)))))

;;; reads an identifier
(define reader:read-identifier
  (lambda (c port continue-with-token)
    (letrec ([loop
	      (lambda (cs)
		(reader:peek-char
		  port
		  (lambda (c)
		    (if (char:separator? c)
			(continue-with-token
			  (string->symbol
			    (apply string (reverse cs))))
			(begin
			  (read-char port)
			  (loop (cons c cs)))))
		  (lambda (eof-object)
		    (continue-with-token
		      (string->symbol
			(apply string (reverse cs)))))))])
      (loop (cons c '())))))

;;; reads a Scheme character
(define reader:read-character
  (lambda (port continue-with-token)
    (reader:read-char
      port
      (lambda (c)
	(case c
	  [(#\n)
	   (let ([c (peek-char port)])
	     (cond
	       [(char:separator? c)
		(continue-with-token #\n)]
	       [(char=? c #\e)
		(begin
		  (read-char port)
		  (reader:read-char
		    port
		    (lambda (c)
		      (if (char=? c #\w)
			  (reader:read-char
			    port
			    (lambda (c)
			      (if (char=? c #\l)
				  (reader:read-char
				    port
				    (lambda (c)
				      (if (char=? c #\i)
					  (reader:read-char
					    port
					    (lambda (c)
					      (if (char=? c #\n)
						  (reader:read-char
						    port
						    (lambda (c)
						      (if (char=? c #\e)
							  (reader:read-char
							    port
							    (lambda (c)
							      (if (char:separator? c)
								  (continue-with-token char:newline)
								  (error 'read
									 "Invalid character name #\\newline~s"
									 c)))
							    (lambda (eof-object)
							      (continue-with-token char:newline)))
							  (error 'read
								 "Invalid character name #\\newlin~s"
								 c)))
						    (lambda (eof-object)
						      (error 'read
							     "Invalid character name #\\newlin")))
						  (error 'read
							 "Invalid character name #\\newli~s"
							 c)))
					    (lambda (eof-object)
					      (error 'read
						     "Invalid character name #\\newli")))
					  (error 'read
						 "Invalid character name #\\newl~s"
						 c)))
				    (lambda (eof-object)
				      (error 'read
					     "Invalid character name #\\newl")))
				  (error 'read
					 "Invalid character name #\\new~s"
					 c)))
			    (lambda (eof-object)
			      (error 'read
				     "Invalid character name #\\new")))
			  (error 'read
				 "Invalid character name #\\ne~s"
				 c)))
		    (lambda (eof-object)
		      (error 'read
			     "Invalid character name #\\ne"))))]
	       [else
		(error 'read "Invalid character name #\\n~s" c)]))]
	[(#\s)
	 (let ([c (peek-char port)])
	     (cond
	       [(char:separator? c)
		(continue-with-token #\s)]
	       [(char=? c #\p)
		(begin
		  (read-char port)
		  (reader:read-char
		    port
		    (lambda (c)
		      (if (char=? c #\a)
			  (reader:read-char
			    port
			    (lambda (c)
			      (if (char=? c #\c)
				  (reader:read-char
				    port
				    (lambda (c)
				      (if (char=? c #\e)
					  (reader:read-char
					    port
					    (lambda (c)
					      (if (char:separator? c)
						  (continue-with-token char:space)
						  (error 'read
							 "Invalid character name #\\space~s"
							 c)))
					    (lambda (eof-object)
					      (continue-with-token char:space)))
				  (error 'read
					 "Invalid character name #\\spa~s"
					 c)))
				    (lambda (eof-object)
				      (error 'read
					     "Invalid character name #\\spa")))
				  (error 'read
					 "Invalid character name #\\spa~s"
					 c)))
			    (lambda (eof-object)
			      (error 'read
				     "Invalid character name #\\spa")))
			  (error 'read
				 "Invalid character name #\\sp~s"
				 c)))
		    (lambda (eof-object)
		      (error 'read
			     "Invalid character name #\\sp"))))]
	       [else
		(error 'read "Invalid character name #\\s~s" c)]))]
	[(#\t)
	 (let ([c (peek-char port)])
	     (cond
	       [(char:separator? c)
		(continue-with-token #\t)]
	       [(char=? c #\a)
		(begin
		  (read-char port)
		  (reader:read-char
		    port
		    (lambda (c)
		      (if (char=? c #\b)
			  (reader:peek-char
			    port
			    (lambda (c)
			      (if (char:separator? c)
				  (continue-with-token char:tab)
				  (error 'read
					 "Invalid character name #\\tab~s"
					 c)))
			    (lambda (eof-object)
			      (continue-with-token char:tab)))
			  (error 'read
				 "Invalid character name #\\ta~s"
				 c)))
		    (lambda (eof-object)
		      (error 'read
			     "Invalid character name #\\ta"))))]
	       [else
		(error 'read "Invalid character name #\\t~s" c)]))]
	[else
	 (reader:peek-char
	   port
	   (lambda (c2)
	     (if (char:separator? c2)
		 (continue-with-token c)
		 (error 'read
			"Invalid character name #\\~s~s" c c2)))
	   (lambda (eof-object)
	     (continue-with-token c)))]))
      (lambda (eof-object)
	(error 'read "end of input stream")))))

;;; reads a string, including quoted characters (with #\\)
(define reader:read-string
  (lambda (port continue-with-token)
    (letrec ([loop
	      (lambda (cs)
		(reader:read-char
		  port
		  (lambda (c)
		    (case c
		      [(#\")
		       (continue-with-token
			 (apply string (reverse cs)))]
		      [(#\\)
		       (reader:read-char
			 port
			 (lambda (c)
			   (loop (cons c cs)))
			 (lambda (eof-object)
			   (error 'read "end of input stream")))]
		      [else
		       (loop (cons c cs))]))
		  (lambda (eof-object)
		    (error 'read "end of input stream"))))])
      (loop '()))))

;;;;;;;;;;

(define reader:read
  (lambda (port continue-with-token continue-with-eof-object)
    (letrec ([read-round-parens
	      (lambda (continue-with-token)
		(reader:skip-whitespace-peek
		  port
		  (lambda (c)
		    (case c
		      [(#\))
		       (begin
			 (read-char port)
			 (continue-with-token '()))]
		      [else
		       (reader:read
			 port
			 (lambda (token)
			   (read-round-parens
			     (lambda (tokens)
			       (continue-with-token
				 (cons token tokens)))))
			 (lambda (eof-object)
			   (error 'read "end of input stream")))]))
		  (lambda (eof-object)
		    (error 'read "end of input stream"))))]
	     [read-square-parens
	      (lambda (continue-with-token)
		(reader:skip-whitespace-peek
		  port
		  (lambda (c)
		    (case c
		      [(#\])
		       (begin
			 (read-char port)
			 (continue-with-token '()))]
		      [else
		       (reader:read
			 port
			 (lambda (token)
			   (read-square-parens
			     (lambda (tokens)
			       (continue-with-token
				 (cons token tokens)))))
			 (lambda (eof-object)
			   (error 'read "end of input stream")))]))
		  (lambda (eof-object)
		    (error 'read "end of input stream"))))])
      (reader:skip-whitespace
	port
	(lambda (c)
	  (case c
	    [(#\()
	     (read-round-parens continue-with-token)]
	    [(#\[)
	     (read-square-parens continue-with-token)]
	    [(#\) #\])
	     (error 'read "unexpected ~s" c)]
	    [(#\')
	     (reader:read
	       port
	       (lambda (token)
		 (continue-with-token (list 'quote token)))
	       (lambda (eof-object)
		 (error 'read "end of input stream")))]
	    [(#\`)
	     (reader:read
	       port
	       (lambda (token)
		 (continue-with-token (list 'quasiquote token)))
	       (lambda (eof-object)
		 (error 'read "end of input stream")))]
	    [(#\,)
	     (if (char=? (peek-char port) #\@)
		 (begin
		   (read-char port)
		   (reader:read
		     port
		     (lambda (token)
		       (continue-with-token (list 'unquote-splicing token)))
		     (lambda (eof-object)
		       (error 'read "end of input stream"))))
		 (reader:read
		   port
		   (lambda (token)
		     (continue-with-token (list 'unquote token)))
		   (lambda (eof-object)
		     (error 'read "end of input stream"))))]
	    [(#\#)
	     (reader:read-char
	       port
	       (lambda (c)
		 (case c
		   [(#\t #\T)
		    (reader:peek-char
		      port
		      (lambda (c)
			(if (char:separator? c)
			    (continue-with-token #t)
			    (error 'read "unknown #t~s sequence" c)))
		      (lambda (eof-object)
			(continue-with-token #t)))]
		   [(#\f #\F)
		    (reader:peek-char
		      port
		      (lambda (c)
			(if (char:separator? c)
			    (continue-with-token #f)
			    (error 'read "unknown #f~s sequence" c)))
		      (lambda (eof-object)
			(continue-with-token #f)))]
		   [(#\\)
		    (reader:read-character
		      port
		      continue-with-token)]
		   [else
		    (error 'read "unknown # code: ~s" c)]))
	       (lambda (eof-object)
		 (error 'read "end of input stream")))]
	    [(#\")
	     (reader:read-string
	       port
	       continue-with-token)]
	    [(#\-)
	     (reader:peek-char
	       port
	       (lambda (c)
		 (if (digit? c)
		     (reader:read-num
		       (digit->integer (read-char port))
		       port
		       (lambda (token)
			 (if (integer? token)
			     (continue-with-token (- 0 token))
			     (string->symbol
			       (string-append
				 "-"
				 (symbol->string token))))))
		     (reader:read-identifier
		       #\-
		       port
		       continue-with-token)))
	       (lambda (eof-object)
		 (continue-with-token '-)))]
	    [(#\+)
	     (reader:peek-char
	       port
	       (lambda (c)
		 (if (digit? c)
		     (reader:read-num
		       (digit->integer (read-char port))
		       port
		       continue-with-token)
		     (reader:read-identifier
		       #\+
		       port
		       continue-with-token)))
	       (lambda (eof-object)
		 (continue-with-token '+)))]
	    [else
	     (if (digit? c)
		 (reader:read-num
		   (digit->integer c)
		   port
		   continue-with-token)
		 (reader:read-identifier
		   c
		   port
		   continue-with-token))]))
	continue-with-eof-object))))

(define read
  (lambda <port>
    (let ([<port> (if (null? <port>) (current-input-port) (car <port>))])
      (call/cc
	(lambda (k)
	  (reader:read <port> (lambda (token) token) k))))))

;;;;;;;;;;

;;; end of "read.scm"
