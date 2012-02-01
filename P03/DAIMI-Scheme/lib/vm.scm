;;; DAIMI-Scheme/lib/vm.scm
;;; a virtual machine for compiled DAIMI-Scheme programs
;;; Olivier Danvy <danvy@brics.dk>
;;; November 2002, October 2003


;;;;;;;;;;
;;; The DAIMI-Scheme values:

(define-record (DS-nil))
(define-record (DS-false))
(define-record (DS-true))
(define-record (DS-integer i))
(define-record (DS-char c))
(define-record (DS-string s))
(define-record (DS-symbol s))

(define-record (DS-closure label env-lex unique-tag))
(define-record (DS-primitive name arity procedure))
(define-record (DS-continuation cont))

(define-record (DS-pair a d))
(define-record (DS-vector v))
(define-record (DS-input-port v))
(define-record (DS-output-port v))
(define-record (DS-eof-object))

(define-record (DS-undefined))
(define-record (DS-void))

;;;;;;;;;;

(define DS-nil (make-DS-nil))
(define DS-false (make-DS-false))
(define DS-true (make-DS-true))
(define DS-undefined (make-DS-undefined))
(define DS-void (make-DS-void))

;;;;;;;;;;
;;; DAIMI-Scheme to Scheme conversion:

(define DS2S
  (lambda (v)
    (case-record v
      [(DS-nil)
       '()]
      [(DS-false)
       #f]
      [(DS-true)
       #t]
      [(DS-integer i)
       i]
      [(DS-char c)
       c]
      [(DS-string s)
       s]
      [(DS-symbol s)
       s]
      [(DS-closure label env-lex unique-tag)
       "#<user-defined procedure>"]
      [(DS-primitive name arity procedure)
       (string-append "#<predefined procedure "
		      (string-append (if (string? name) name (symbol->string name))
				     ">"))]
      [(DS-continuation cont)
       "#<continuation procedure>"]
      [(DS-pair a d)
       (cons (DS2S a) (DS2S d))]
      [(DS-vector v)
       (apply vector (map1 DS2S (vector->list v)))]
      [(DS-input-port v)
       "#<input port>"]
      [(DS-output-port v)
       "#<output port>"]
      [(DS-eof-object)
       "#<eof object>"]
      [(DS-void)
       "#<void object>"]
      [else
       (error 'DS2S "not a DAIMI-Scheme value: ~s" v)])))

;;;;;;;;;;
;;; Type-checking operations:

(define check-arity
  (lambda (name arity vec)
    (if (or (= arity -1) (= arity (vector-length vec)))
	'OK
	(error 'check-arity "~s(~s): wrong arity: ~s" name arity vec))))

(define check-type-one
  (lambda (check-type op actual type)
    (let ([tag (vector-ref actual 0)])
      (if (case tag
	    [(DS-nil)
	     #f]
	    [(DS-false)
	     (eqv? type 'boolean)]
	    [(DS-true)
	     (eqv? type 'boolean)]
	    [(DS-integer)
	     (eqv? type 'integer)]
	    [(DS-char)
	     (eqv? type 'char)]
	    [(DS-string)
	     (eqv? type 'string)]
	    [(DS-symbol)
	     (eqv? type 'symbol)]
	    [(DS-closure)
	     (eqv? type 'procedure)]
	    [(DS-primitive)
	     (eqv? type 'procedure)]
	    [(DS-continuation)
	     (eqv? type 'procedure)]
	    [(DS-pair)
	     (eqv? type 'pair)]
	    [(DS-vector)
	     (eqv? type 'vector)]
	    [(DS-input-port)
	     (eqv? type 'input-port)]
	    [(DS-output-port)
	     (eqv? type 'output-port)]
	    [(DS-eof-object)
	     #f]
	    [else
	     (error 'check-type-one "not a DAIMI-Scheme value: ~s" actual)])
	  'OK
	  (error check-type "~s / not a ~s: ~s" op type actual)))))

(define check-type
  (lambda (op actuals type)
    (check-type-one 'check-type op (vector-ref actuals 0) type)))

(define check-type2
  (lambda (op actuals type)
    (check-type-one 'check-type2 op (vector-ref actuals 1) type)))

(define check-types
  (lambda (op actuals type)
    (let ([len (vector-length actuals)])
      (letrec ([walk
		(lambda (i)
		  (if (= i len)
		      'OK
		      (begin
			(check-type-one 'check-types op (vector-ref actuals i) type)
			(walk (+ i 1)))))])
	(walk 0)))))

(define check-DS-proper-list?
  (lambda (v)
    (if (DS-proper-list? v)
	'OK
	(error 'check-DS-proper-list? "not a proper list: ~s" v))))


;;;;;;;;;;
;;; DAIMI-Scheme basic equality predicate:

(define DS-eqv?
  (lambda (x y)
    (case-record x
      [(DS-nil)
       (case-record y
	 [(DS-nil)
	  #t]
	 [else
	  #f])]
      [(DS-false)
       (case-record y
	 [(DS-false)
	  #t]
	 [else
	  #f])]
      [(DS-true)
       (case-record y
	 [(DS-true)
	  #t]
	 [else
	  #f])]
      [(DS-integer ix)
       (case-record y
	 [(DS-integer iy)
	  (= ix iy)]
	 [else
	  #f])]
      [(DS-char cx)
       (case-record y
	 [(DS-char cy)
	  (eqv? cx cy)]
	 [else
	  #f])]
      [(DS-string sx)
       (case-record y
	 [(DS-string sy)
	  (eqv? x y)]
	 [else
	  #f])]
      [(DS-symbol sx)
       (case-record y
	 [(DS-symbol sy)
	  (eqv? sx sy)]
	 [else
	  #f])]
      [(DS-closure labelx env-lexx unique-tagx)
       (case-record y
	 [(DS-closure labely env-lexy unique-tagy)
	  (eqv? unique-tagx unique-tagy)]
	 [else
	  #f])]
      [(DS-primitive namex arityx procedurex)
       (case-record y
	 [(DS-primitive namey arityy procedurey)
	  (eqv? namex namey)]
	 [else
	  #f])]
      [(DS-continuation cx)
       (case-record y
	 [(DS-continuation cy)
	  (eqv? cx cy)]
	 [else
	  #f])]
      [(DS-pair ax dx)
       (case-record y
	 [(DS-pair ay dy)
	  (eqv? x y)]
	 [else
	  #f])]
      [(DS-vector vx)
       (case-record y
	 [(DS-vector vy)
	  (eqv? x y)]
	 [else
	  #f])]
      [(DS-input-port vx)
       (case-record y
	 [(DS-input-port vy)
	  (eqv? x y)]
	 [else
	  #f])]
      [(DS-output-port vx)
       (case-record y
	 [(DS-output-port vy)
	  (eqv? x y)]
	 [else
	  #f])]
      [else
       (error 'DS-eqv? "not a DAIMI-Scheme value: ~s" x)])))

(define vector->DS-list
  (lambda (v)
    (let ([len (vector-length v)])
      (letrec ([loop
		(lambda (offset)
		  (if (= offset len)
		      DS-nil
		      (make-DS-pair (vector-ref v offset)
				    (loop (1+ offset)))))])
	(loop 0)))))

(define DS-list->vector
  (lambda (xs)
    (letrec ([walk (lambda (xs)
		     (case-record xs
		       [(DS-nil)
			'()]
		       [(DS-pair a d)
			(cons a (walk d))]
		       [else
			(error 'DS-list->vector
			       "not a list: ~s" xs)]))])
      (list->vector (walk xs)))))

(define DS-car
  (lambda (v)
    (case-record v
      [(DS-pair a d)
       a]
      [else
       (error 'DS-car "not a pair: ~s" v)])))

(define DS-cdr
  (lambda (v)
    (case-record v
      [(DS-pair a d)
       d]
      [else
       (error 'DS-car "not a pair: ~s" v)])))

(define DS-null?
  (lambda (v)
    (case-record v
      [(DS-nil)
       #t]
      [else
       #f])))

(define DS-pair?
  (lambda (v)
    (case-record v
      [(DS-pair a d)
       #t]
      [else
       #f])))

(define DS-proper-list?
  (lambda (xs)
    (letrec ([walk (lambda (xs1 xs2)
		     (or (DS-null? xs2)
			 (and (DS-pair? xs2)
			      (and (not (eqv? xs1 xs2))
				   (let ([xs2p (DS-cdr xs2)])
				     (or (DS-null? xs2p)
					 (and (DS-pair? xs2p)
					      (walk (DS-cdr xs1)
						    (DS-cdr xs2p)))))))))])
      (or (DS-null? xs)
	  (and (DS-pair? xs)
	       (walk xs (DS-cdr xs)))))))

(define DS-proper-list?_does_not_work_somehow
  (lambda (xs)
    (letrec ([walk (lambda (xs1 xs2)
		     (case-record xs2
		       [(DS-nil)
			#t]
		       [(DS-pair a2 d2)
			(and (not (eqv? xs1 xs2))
			     (case-record d2
			       [(DS-nil)
				#t]
			       [(DS-pair a22 d22)
				(case-record d22
				  [(DS-nil)
				   #t]
				  [(DS-pair a33 d33)
				   (case-record xs1
				     [(DS-pair a1 d1)
				      (walk d1 d33)]
				     [else
				      (error 'DS-proper-list?
					     "im-po-ssi-ble!")])]
				  [else
				   #f])]
			       [else
				#f]))]
		       [else
			#f]))])
      (case-record xs
	[(DS-nil)
	 #t]
	[(DS-pair a d)
	 (walk a d)]
	[else
	 #f]))))


;;;;;;;;;;
;;; The environment of predefined procedures,
;;; in the same order as in predefined.scm

(define env-lib-reference
  (vector
    (make-DS-primitive
      'integer?
      1
      (lambda (actuals resume)
	(begin
	  (case (vector-ref (vector-ref actuals 0) 0)
	    [(DS-integer)
	     (vector-set! aux-res 0 DS-true)]
	    [else
	     (vector-set! aux-res 0 DS-false)])
	  (resume))))
    (make-DS-primitive
      '+
      -1
      (lambda (actuals resume)
	(begin
	  (check-types '+ actuals 'integer)
	  (vector-set! aux-res
		       0
		       (make-DS-integer (fold-vec actuals + 0)))
	  (resume))))
    (make-DS-primitive
      '-
      2
      (lambda (actuals resume)
	(begin
	  (check-types '- actuals 'integer)
	  (vector-set! aux-res
		       0
		       (make-DS-integer
			 (- (vector-ref (vector-ref actuals 0) 1)
			    (vector-ref (vector-ref actuals 1) 1))))
	  (resume))))
    (make-DS-primitive
      '*
      -1
      (lambda (actuals resume)
	(begin
	  (check-types '* actuals 'integer)
	  (vector-set! aux-res
		       0
		       (make-DS-integer (fold-vec actuals * 1)))
	  (resume))))
    (make-DS-primitive
      'quotient
      2
      (lambda (actuals resume)
	(begin
	  (check-types 'quotient actuals 'integer)
	  (vector-set! aux-res
		       0
		       (make-DS-integer
			 (quotient (vector-ref (vector-ref actuals 0) 1)
				   (vector-ref (vector-ref actuals 1) 1))))
	  (resume))))
    (make-DS-primitive
      'remainder
      2
      (lambda (actuals resume)
	(begin
	  (check-types 'remainder actuals 'integer)
	  (vector-set! aux-res
		       0
		       (make-DS-integer
			 (remainder (vector-ref (vector-ref actuals 0) 1)
				    (vector-ref (vector-ref actuals 1) 1))))
	  (resume))))
    (make-DS-primitive
      '<
      2
      (lambda (actuals resume)
	(begin
	  (check-types '< actuals 'integer)
	  (vector-set! aux-res
		       0
		       (if (< (vector-ref (vector-ref actuals 0) 1)
			      (vector-ref (vector-ref actuals 1) 1))
			   DS-true
			   DS-false))
	  (resume))))
    (make-DS-primitive
      '<=
      2
      (lambda (actuals resume)
	(begin
	  (check-types '<= actuals 'integer)
	  (vector-set! aux-res
		       0
		       (if (<= (vector-ref (vector-ref actuals 0) 1)
			       (vector-ref (vector-ref actuals 1) 1))
			   DS-true
			   DS-false))
	  (resume))))
    (make-DS-primitive
      '=
      2
      (lambda (actuals resume)
	(begin
	  (check-types '= actuals 'integer)
	  (vector-set! aux-res
		       0
		       (if (= (vector-ref (vector-ref actuals 0) 1)
			      (vector-ref (vector-ref actuals 1) 1))
			   DS-true
			   DS-false))
	  (resume))))
    (make-DS-primitive
      '>=
      2
      (lambda (actuals resume)
	(begin
	  (check-types '>= actuals 'integer)
	  (vector-set! aux-res
		       0
		       (if (>= (vector-ref (vector-ref actuals 0) 1)
			       (vector-ref (vector-ref actuals 1) 1))
			   DS-true
			   DS-false))
	  (resume))))
    (make-DS-primitive
      '>
      2
      (lambda (actuals resume)
	(begin
	  (check-types '> actuals 'integer)
	  (vector-set! aux-res
		       0
		       (if (> (vector-ref (vector-ref actuals 0) 1)
			      (vector-ref (vector-ref actuals 1) 1))
			   DS-true
			   DS-false))
	  (resume))))
    (make-DS-primitive
      'boolean?
      1
      (lambda (actuals resume)
	(begin
	  (case (vector-ref (vector-ref actuals 0) 0)
	    [(DS-true)
	     (vector-set! aux-res 0 DS-true)]
	    [(DS-false)
	     (vector-set! aux-res 0 DS-true)]
	    [else
	     (vector-set! aux-res 0 DS-false)])
	  (resume))))
    (make-DS-primitive
      'symbol?
      1
      (lambda (actuals resume)
	(begin
	  (case (vector-ref (vector-ref actuals 0) 0)
	    [(DS-symbol)
	     (vector-set! aux-res 0 DS-true)]
	    [else
	     (vector-set! aux-res 0 DS-false)])
	  (resume))))
    (make-DS-primitive
      'char?
      1
      (lambda (actuals resume)
	(begin
	  (case (vector-ref (vector-ref actuals 0) 0)
	    [(DS-char)
	     (vector-set! aux-res 0 DS-true)]
	    [else
	     (vector-set! aux-res 0 DS-false)])
	  (resume))))
    (make-DS-primitive
      'char->integer
      1
      (lambda (actuals resume)
	(begin
	  (check-type 'char->integer actuals 'char)
	  (vector-set! aux-res
		       0
		       (make-DS-integer
			 (char->integer (vector-ref (vector-ref actuals 0) 1))))
	  (resume))))
    (make-DS-primitive
      'integer->char
      1
      (lambda (actuals resume)
	(begin
	  (check-type 'integer->char actuals 'integer)
	  (vector-set! aux-res
		       0
		       (make-DS-char
			 (integer->char (vector-ref (vector-ref actuals 0) 1))))
	  (resume))))
    (make-DS-primitive
      'string
      -1
      (lambda (actuals resume)
	(begin
	  (check-types 'string actuals 'char)
	  (vector-set! aux-res
		       0
		       (make-DS-string
			 (apply string
				(map1 (lambda (actual)
					(vector-ref actual 1))
				      (vector->list actuals)))))
	  (resume))))
    (make-DS-primitive
      'make-string
      2
      (lambda (actuals resume)
	(begin
	  (check-type 'make-string actuals 'integer)
	  (check-type2 'make-string actuals 'char)
	  (vector-set! aux-res
		       0
		       (make-DS-string
			 (make-string
			   (vector-ref (vector-ref actuals 0) 1)
			   (vector-ref (vector-ref actuals 1) 1))))
	  (resume))))
    (make-DS-primitive
      'string?
      1
      (lambda (actuals resume)
	(begin
	  (case (vector-ref (vector-ref actuals 0) 0)
	    [(DS-string)
	     (vector-set! aux-res 0 DS-true)]
	    [else
	     (vector-set! aux-res 0 DS-false)])
	  (resume))))
    (make-DS-primitive
      'string-length
      1
      (lambda (actuals resume)
	(begin
	  (check-type 'string-length actuals 'string)
	  (vector-set! aux-res
		       0
		       (make-DS-integer
			 (string-length
			   (vector-ref (vector-ref actuals 0) 1))))
	  (resume))))
    (make-DS-primitive
      'string-append
      2
      (lambda (actuals resume)
	(begin
	  (check-types 'string-append actuals 'string)
	  (vector-set! aux-res
		       0
		       (make-DS-string
			 (string-append (vector-ref (vector-ref actuals 0) 1)
					(vector-ref (vector-ref actuals 1) 1))))
	  (resume))))
    (make-DS-primitive
      'string=?
      2
      (lambda (actuals resume)
	(begin
	  (check-types 'string-=? actuals 'string)
	  (vector-set! aux-res
		       0
		       (if (string=? (vector-ref (vector-ref actuals 0) 1)
				     (vector-ref (vector-ref actuals 1) 1))
			   DS-true
			   DS-false))
	  (resume))))
    (make-DS-primitive
      'string-ref
      2
      (lambda (actuals resume)
	(begin
	  (check-type 'string-ref actuals 'string)
	  (check-type2 'string-ref actuals 'integer)
	  (vector-set! aux-res
		       0
		       (let ([s (vector-ref (vector-ref actuals 0) 1)]
			     [i (vector-ref (vector-ref actuals 1) 1)])
			 (if (and (>= i 0) (< i (string-length s)))
			     (make-DS-char (string-ref s i))
			     (error 'primitive-string-ref
				    "index out of bounds: ~s and ~s" s i))))
	  (resume))))
    (make-DS-primitive
      'string->symbol
      1
      (lambda (actuals resume)
	(begin
	  (check-type 'string->symbol actuals 'string)
	  (vector-set! aux-res
		       0
		       (make-DS-symbol
			 (string->symbol
			   (vector-ref (vector-ref actuals 0) 1))))
	  (resume))))
    (make-DS-primitive
      'symbol->string
      1
      (lambda (actuals resume)
	(begin
	  (check-type 'symbol->string actuals 'symbol)
	  (vector-set! aux-res
		       0
		       (make-DS-string
			 (symbol->string
			   (vector-ref (vector-ref actuals 0) 1))))
	  (resume))))
    (make-DS-primitive
      'pair?
      1
      (lambda (actuals resume)
	(begin
	  (case (vector-ref (vector-ref actuals 0) 0)
	    [(DS-pair)
	     (vector-set! aux-res 0 DS-true)]
	    [else
	     (vector-set! aux-res 0 DS-false)])
	  (resume))))
    (make-DS-primitive
      'cons
      2
      (lambda (actuals resume)
	(begin
	  (vector-set! aux-res
		       0
		       (make-DS-pair 
			 (vector-ref actuals 0)
			 (vector-ref actuals 1)))
	  (resume))))
    (make-DS-primitive
      'car
      1
      (lambda (actuals resume)
	(begin
	  (check-type 'car actuals 'pair)
	  (vector-set! aux-res
		       0
		       (vector-ref (vector-ref actuals 0) 1))
	  (resume))))
    (make-DS-primitive
      'cdr
      1
      (lambda (actuals resume)
	(begin
	  (check-type 'cdr actuals 'pair)
	  (vector-set! aux-res
		       0
		       (vector-ref (vector-ref actuals 0) 2))
	  (resume))))
    (make-DS-primitive
      'set-car!
      2
      (lambda (actuals resume)
	(begin
	  (check-type 'set-car! actuals 'pair)
	  (vector-set! (vector-ref actuals 0)
		       1
		       (vector-ref actuals 1))
	  (vector-set! aux-res
		       0
		       DS-void)
	  (resume))))
    (make-DS-primitive
      'set-cdr!
      2
      (lambda (actuals resume)
	(begin
	  (check-type 'set-car! actuals 'pair)
	  (vector-set! (vector-ref actuals 0)
		       2
		       (vector-ref actuals 1))
	  (vector-set! aux-res
		       0
		       DS-void)
	  (resume))))
    (make-DS-primitive
      'null?
      1
      (lambda (actuals resume)
	(begin
	  (case (vector-ref (vector-ref actuals 0) 0)
	    [(DS-nil)
	     (vector-set! aux-res 0 DS-true)]
	    [else
	     (vector-set! aux-res 0 DS-false)])
	  (resume))))
    (make-DS-primitive
      'vector
      -1
      (lambda (actuals resume)
	(begin
	  (vector-set! aux-res
		       0
		       (make-DS-vector
			 (vector-copy actuals)))
	  (resume)))) 
    (make-DS-primitive
      'make-vector
      2
      (lambda (actuals resume)
	(begin
	  (check-type 'make-vector actuals 'integer)
	  (vector-set! aux-res
		       0
		       (make-DS-vector
			 (make-vector
			   (vector-ref (vector-ref actuals 0) 1)
			   (vector-ref actuals 1))))
	  (resume))))
    (make-DS-primitive
      'vector?
      1
      (lambda (actuals resume)
	(begin
	  (case (vector-ref (vector-ref actuals 0) 0)
	    [(DS-vector)
	     (vector-set! aux-res 0 DS-true)]
	    [else
	     (vector-set! aux-res 0 DS-false)])
	  (resume))))
    (make-DS-primitive
      'vector-length
      1
      (lambda (actuals resume)
	(begin
	  (check-type 'vector-length actuals 'vector)
	  (vector-set! aux-res
		       0
		       (make-DS-integer
			 (vector-length
			   (vector-ref (vector-ref actuals 0) 1))))
	  (resume))))
    (make-DS-primitive
      'vector-ref
      2
      (lambda (actuals resume)
	(begin
	  (check-type 'vector-ref actuals 'vector)
	  (check-type2 'vector-ref actuals 'integer)
	  (vector-set! aux-res
		       0
		       (let ([s (vector-ref (vector-ref actuals 0) 1)]
			     [i (vector-ref (vector-ref actuals 1) 1)])
			 (if (and (>= i 0) (< i (vector-length s)))
			     (vector-ref s i)
			     (error 'primitive-vector-ref
				    "index out of bounds: ~s and ~s" s i))))
	  (resume))))
    (make-DS-primitive
      'vector-set!
      3
      (lambda (actuals resume)
	(begin
	  (check-type 'vector-set! actuals 'vector)
	  (check-type2 'vector-set! actuals 'integer)
	  (let ([s (vector-ref (vector-ref actuals 0) 1)]
		[i (vector-ref (vector-ref actuals 1) 1)])
	    (if (and (>= i 0) (< i (vector-length s)))
		(vector-set! s i (vector-ref actuals 2))
		(error 'primitive-vector-set!
		       "index out of bounds: ~s and ~s" s i)))
	  (vector-set! aux-res
		       0
		       DS-void)
	  (resume))))
    (make-DS-primitive
      'procedure?
      1
      (lambda (actuals resume)
	(begin
	  (case (vector-ref (vector-ref actuals 0) 0)
	    [(DS-closure)
	     (vector-set! aux-res 0 DS-true)]
	    [(DS-primitive)
	     (vector-set! aux-res 0 DS-true)]
	    [(DS-continuation)
	     (vector-set! aux-res 0 DS-true)]
	    [else
	     (vector-set! aux-res 0 DS-false)])
	  (resume))))
    (make-DS-primitive
      'apply
      2
      'apply)
    (make-DS-primitive
      'eqv?
      2
      (lambda (actuals resume)
	(begin
	  (vector-set! aux-res
		       0
		       (if (DS-eqv? (vector-ref actuals 0)
				    (vector-ref actuals 1))
			   DS-true
			   DS-false))
	  (resume))))
    (make-DS-primitive
      'call/cc
      1
      'call/cc)
    (make-DS-primitive
      'exit
      1
      (lambda (actuals resume)
	(begin
	  (check-type 'exit actuals 'integer)
	  (vector-ref actuals 0))))
    (make-DS-primitive
      'open-input-file
      1
      (lambda (actuals resume)
	(begin
	  (check-type 'open-input-file actuals 'string)
	  (vector-set! aux-res
		       0
		       (make-DS-input-port
			 (open-input-file
			   (vector-ref (vector-ref actuals 0) 1))))
	  (resume))))
    (make-DS-primitive
      'input-port?
      1
      (lambda (actuals resume)
	(begin
	  (case (vector-ref (vector-ref actuals 0) 0)
	    [(DS-input-port)
	     (vector-set! aux-res 0 DS-true)]
	    [else
	     (vector-set! aux-res 0 DS-false)])
	  (resume))))
    (make-DS-primitive
      'close-input-port
      1
      (lambda (actuals resume)
	(begin
	  (check-type 'close-input-port actuals 'input-port)
	  (close-input-port (vector-ref (vector-ref actuals 0) 1))
	  (vector-set! aux-res
		       0
		       DS-void)
	  (resume))))
    (make-DS-primitive
      'current-input-port
      0
      (lambda (actuals resume)
	(begin
	  (vector-set! aux-res
		       0
		       (make-DS-input-port (current-input-port)))
	  (resume))))
    (make-DS-primitive
      'read-char
      1
      (lambda (actuals resume)
	(begin
	  (check-type 'read-char actuals 'input-port)
	  (vector-set! aux-res
		       0
		       (let ([c (read-char
				  (vector-ref (vector-ref actuals 0) 1))])
			 (if (eof-object? c)
			     (make-DS-eof-object)
			     (make-DS-char c))))
	  (resume))))
    (make-DS-primitive
      'peek-char
      1
      (lambda (actuals resume)
	(begin
	  (check-type 'peek-char actuals 'input-port)
	  (vector-set! aux-res
		       0
		       (let ([c (peek-char
				  (vector-ref (vector-ref actuals 0) 1))])
			 (if (eof-object? c)
			     (make-DS-eof-object)
			     (make-DS-char c))))
	  (resume))))
    (make-DS-primitive
      'eof-object?
      1
      (lambda (actuals resume)
	(begin
	  (case (vector-ref (vector-ref actuals 0) 0)
	    [(DS-eof-object)
	     (vector-set! aux-res 0 DS-true)]
	    [else
	     (vector-set! aux-res 0 DS-false)])
	  (resume))))
    (make-DS-primitive
      'open-output-file
      1
      (lambda (actuals resume)
	(begin
	  (check-type 'open-output-file actuals 'string)
	  (vector-set! aux-res
		       0
		       (make-DS-output-port
			 (open-output-file
			   (vector-ref (vector-ref actuals 0) 1))))
	  (resume))))
    (make-DS-primitive
      'output-port?
      1
      (lambda (actuals resume)
	(begin
	  (case (vector-ref (vector-ref actuals 0) 0)
	    [(DS-output-port)
	     (vector-set! aux-res 0 DS-true)]
	    [else
	     (vector-set! aux-res 0 DS-false)])
	  (resume))))
    (make-DS-primitive
      'close-output-port
      1
      (lambda (actuals resume)
	(begin
	  (check-type 'close-output-port actuals 'output-port)
	  (close-output-port (vector-ref (vector-ref actuals 0) 1))
	  (vector-set! aux-res
		       0
		       DS-void)
	  (resume))))
    (make-DS-primitive
      'current-output-port
      0
      (lambda (actuals resume)
	(begin
	  (vector-set! aux-res
		       0
		       (make-DS-output-port (current-output-port)))
	  (resume))))
    (make-DS-primitive
      'write-char
      2
      (lambda (actuals resume)
	(begin
	  (check-type 'write-char actuals 'char)
	  (check-type2 'write-char actuals 'output-port)
	  (write-char (vector-ref (vector-ref actuals 0) 1)
		      (vector-ref (vector-ref actuals 1) 1))
	  (vector-set! aux-res
		       0
		       DS-void)
	  (resume))))
    ))


;;;;;;;;;;
;;; 

(define fold-vec
  (lambda (vec op a)
    (let ([len (vector-length vec)])
      (letrec ([loop
		(lambda (i a)
		  (if (= i len)
		      a
		      (loop (+ i 1)
			    (op (vector-ref (vector-ref vec i) 1) a))))])
	(loop 0 a)))))

(define vector-copy
  (lambda (v)
    (let* ([len (vector-length v)]
	   [w (make-vector len '())])
      (letrec ([walk
		(lambda (n)
		  (if (negative? n)
		      w
		      (begin
			(vector-set! w n (vector-ref v n))
			(walk (- n 1)))))])
	(walk (- len 1))))))

(define vector-copy-prefix
  (lambda (v len)
    (let ([w (make-vector len '())])
      (letrec ([walk
		(lambda (n)
		  (if (negative? n)
		      w
		      (begin
			(vector-set! w n (vector-ref v n))
			(walk (- n 1)))))])
	(walk (- len 1))))))

(define vector-restore-prefix!
  (lambda (v w)
    (let ([len (vector-length w)])
      (letrec ([walk
		(lambda (n)
		  (if (= n len)
		      v
		      (begin
			(vector-set! v n (vector-ref w n))
			(walk (+ n 1)))))])
	(walk 0)))))


;;;;;;;;;;
;;; 

(define env-lib
  (vector))

(define env-glo
  (vector))

(define env-tmp
  (vector))

(define aux-res
  (vector))


;;;;;;;;;;
;;; 

(define table-lam-arity
  (vector))

(define table-lam-entry
  (vector))

(define table-code-rator
  (vector))

(define table-code-rands
  (vector))


;;;;;;;;;;
;;; Compilation of symbolic labels into offsets:

(define crossref
  (lambda (the-code k)
    (let* ([the-labels
	    (list->vector
	      (sort
		(lambda (x y)
		  (string<? (car x) (car y)))
		(letrec ([traverse
			  (lambda (is n)
			    (if (null? is)
				'()
				(let ([i (car is)]
				      [is (cdr is)])
				  (case (car i)
				    [(label)
				     (cons (cons (symbol->string (cadr i))
						 n)
					   (traverse is n))]
				    [(nop)
				     (traverse is n)]
				    [else
				     (traverse is (1+ n))]))))])
		  (traverse the-code 0))))]
	   [the-number-of-labels (vector-length the-labels)]
	   [maxi (1- the-number-of-labels)]
	   [the-number-of-instructions (- (length the-code)
					  the-number-of-labels)]
	   [the-offsets (make-vector (vector-length the-labels) -1)]
	   [get-ref
	    (lambda (label)
	      (let ([s (symbol->string label)])
		(letrec ([loop
			  (lambda (mini maxi)
			    (if (< (- maxi mini) 3)
				(cond
				  [(string=? (car (vector-ref the-labels
							      mini))
					     s)
				   (cdr (vector-ref the-labels mini))]
				  [(string=? (car (vector-ref the-labels
							      (+ mini 1)))
					     s)
				   (cdr (vector-ref the-labels (+ mini 1)))]
				  [(string=? (car (vector-ref the-labels
							      (+ mini 2)))
					     s)
				   (cdr (vector-ref the-labels (+ mini 2)))]
				  [(string=? (car (vector-ref the-labels
							      (+ mini 3)))
					     s)
				   (cdr (vector-ref the-labels (+ mini 3)))]
				  [else
				   (error 'crossref
					  "label not found: ~s"
					  label)])
				(let ([median (quotient (+ mini maxi) 2)])
				  (if (string<? (car (vector-ref the-labels
								 median))
						s)
				      (loop median maxi)
				      (loop mini median)))))])
		  (loop 0 maxi))))])
      (letrec ([loop
		(lambda (is pc)
		  (if (null? is)
		      (if (= pc the-number-of-instructions)
			  (k get-ref)
			  (error 'crossref
				 "mismatching numbers: ~s and ~s"
				 pc
				 the-number-of-instructions))
		      (let ([i (car is)] [is (cdr is)])
			(case (car i)
			  [(label)
			   (loop is pc)]
			  [(jump)
			   (begin
			     (vector-set! table-code-rator pc 'jump)
			     (vector-set! table-code-rands pc (get-ref
								(cadr i)))
			     (loop is (1+ pc)))]
			  [(jump-if-false)
			   (begin
			     (vector-set! table-code-rator
					  pc
					  'jump-if-false)
			     (let ([rands (list->vector (cdr i))])
			       (begin
				 (vector-set! rands
					      2
					      (get-ref (vector-ref rands 2)))
				 (vector-set! table-code-rands pc rands)))
			     (loop is (1+ pc)))]
			  [(new-vec)
			   (begin
			     (vector-set! table-code-rator pc 'new-vec)
			     (vector-set! table-code-rands pc (cadr i))
			     (loop is (1+ pc)))]
			  [(move load call tail-call extend return)
			   (begin
			     (vector-set! table-code-rator pc (car i))
			     (vector-set! table-code-rands pc (list->vector
								(cdr i)))
			     (loop is (1+ pc)))]
			  [else
			   (error 'crossref
				  "Illegal instruction: ~s"
				  i)]))))])
	(begin
	   (set! table-code-rator
		 (make-vector (+ the-number-of-instructions 1) '()))
	   (vector-set! table-code-rator the-number-of-instructions 'halt)
	   (set! table-code-rands
		 (make-vector (+ the-number-of-instructions 1) '()))
	   (vector-set! table-code-rands the-number-of-instructions (vector))
	   (loop the-code 0))))))


;;;;;;;;;;
;;; Fetch-decode-execute loop:

(define *tick-tock* 0)

(define tick-tock
  (lambda ()
    (if verbose
	(if (< *tick-tock* 1000)
	    (set! *tick-tock* (+ *tick-tock* 1))
	    (begin
	      (display "." (current-output-port))
	      (set! *tick-tock* 0)))
	'())))


;;;;;;;;;;
;;; 

(define-record (Control-frame ip env-lex tmp cont))
(define-record (Control-frame-initial))

(define run-instructions
  (lambda ()
    (letrec ([loop
	      (lambda (ip env-lex vec cont)
		;;; ip == instruction pointer in table-code-rator/rands
		;;; env-lex == current lexical environment
		;;; vec == auxiliary
		;;; cont == control register
		(begin
		  (if verbose
		      (case trace-vm
			[(0)
			 "Shhh"]
			[(1)
			 (begin
			   (write ip)
			   (display ": ")
			   (write (cons (vector-ref table-code-rator ip)
					(let ([rand (vector-ref table-code-rands ip)])
					  (if (vector? rand)
					      (vector->list rand)
					      (list rand))))
				  (current-output-port))
			   (if (and (eqv? (vector-ref table-code-rator ip)
					  'check-and-jump)
				    (eqv? (vector-ref
					    (vector-ref table-code-rands ip)
					    0)
					  'lib))
			       (begin
				 (write-char (integer->char 9)
					     (current-output-port))
				 (display "; " (current-output-port))
				 (write (list-ref predefined-procedures
						  (vector-ref
						    (vector-ref
						      table-code-rands
						      ip)
						    1))))
			       "ikke noget")
			   (newline (current-output-port)))]
			[(2)
			 (begin
			   (newline (current-output-port))
			   (write (cons (vector-ref table-code-rator ip)
					(let ([rand (vector-ref table-code-rands ip)])
					  (if (vector? rand)
					      (vector->list rand)
					      (list rand))))
				  (current-output-port))
			   (newline (current-output-port))
			   (write (map (lambda (v) (map DS2S (vector->list v))) env-lex) (current-output-port))
			   (newline (current-output-port))
			   (write (map DS2S (vector->list env-tmp)) (current-output-port))
			   (newline (current-output-port))
			   (write cont (current-output-port))
			   (newline (current-output-port)))]
			[else
			 (tick-tock)])
		      (tick-tock))
		  (let ([rands (vector-ref table-code-rands ip)])
		    (case (vector-ref table-code-rator ip)
		      [(jump)
		       (loop rands env-lex vec cont)]
		      [(jump-if-false)
		       (if (case-record (vector-ref
					  (let ([kind (vector-ref rands 0)])
					    (case kind
					      [(res)
					       aux-res]
					      [(tmp)
					       env-tmp]
					      [(glo)
					       env-glo]
					      [(lib)
					       env-lib]
					      [else
					       (list-ref env-lex kind)]))
					  (vector-ref rands 1))
			     [(DS-false)
			      #f]
			     [else
			      #t])
			   (loop (1+ ip) env-lex vec cont)
			   (loop (vector-ref rands 2) env-lex vec cont))]
		      [(new-vec)
		       (loop (1+ ip)
			     env-lex
			     (make-vector (vector-ref table-code-rands ip)
					  DS-nil)
			     cont)]
		      [(extend)
		       (loop (+ ip 1) (cons vec env-lex) vec cont)]
		      [(move)
		       (begin
			 (vector-set!
			   (let ([kind (vector-ref rands 2)])
			     (case kind
			       [(res)
				aux-res]
			       [(tmp)
				env-tmp]
			       [(glo)
				env-glo]
			       [(lib)
				env-lib]
			       [(vec)
				vec]
			       [else
				(list-ref env-lex kind)]))
			   (vector-ref rands 3)
			   (vector-ref
			     (let ([kind (vector-ref rands 0)])
			       (case kind
				 [(res)
				  aux-res]
				 [(tmp)
				  env-tmp]
				 [(glo)
				  env-glo]
				 [(lib)
				  env-lib]
				 [else
				  (list-ref env-lex kind)]))
			     (vector-ref rands 1)))
			 (loop (1+ ip) env-lex vec cont))]
		      [(load)
		       (begin
			 (vector-set!
			   (let ([kind (vector-ref rands 2)])
			     (case kind
			       [(res)
				aux-res]
			       [(tmp)
				env-tmp]
			       [(glo)
				env-glo]
			       [(lib)
				env-lib]
			       [(vec)
				vec]
			       [else
				(list-ref env-lex kind)]))
			   (vector-ref rands 3)
			   (let ([kind (vector-ref rands 0)])
			     (case kind
			       [(nil)
				DS-nil]
			       [(bool)
				(let ([offset (vector-ref rands 1)])
				  (cond
				    [(= offset 0)
				     DS-false]
				    [(= offset 1)
				     DS-true]
				    [else
				     (error 'run
					    "not a boolean: ~s" offset)]))]
			       [(int)
				(make-DS-integer (vector-ref rands 1))]
			       [(char)
				(make-DS-char (integer->char
						(vector-ref rands 1)))]
			       [(str)
				(make-DS-string (vector-ref rands 1))]
			       [(sym)
				(make-DS-symbol (vector-ref rands 1))]
			       [(close-flat)
				(make-DS-closure (vector-ref rands 1)
						 (list vec)
						 (gensym! "flat-closure%"))]
			       [(close-deep)
				(make-DS-closure (vector-ref rands 1)
						 env-lex
						 (gensym! "deep-closure%"))]
			       ;[(vector)
			       ;(make-DS-vector (make-vector (vector-ref rands 1) DS-nil))]
			       ;[(pair)
			       ;(make-DS-vector (make-vector 2 DS-nil))]
			       [(void)
				DS-void]
			       [else
				(list-ref env-lex kind)])))
			 (loop (1+ ip) env-lex vec cont))]
		      [(tail-call)
		       (application (vector-ref
				      (let ([kind (vector-ref rands 0)])
					(case kind
					  [(res)
					   aux-res]
					  [(tmp)
					   env-tmp]
					  [(glo)
					   env-glo]
					  [(lib)
					   env-lib]
					  [else
					   (list-ref env-lex kind)]))
				      (vector-ref rands 1))
				    vec
				    cont)]
		      [(call)
		       (application (vector-ref
				      (let ([kind (vector-ref rands 0)])
					(case kind
					  [(res)
					   aux-res]
					  [(tmp)
					   env-tmp]
					  [(glo)
					   env-glo]
					  [(lib)
					   env-lib]
					  [else
					   (list-ref env-lex kind)]))
				      (vector-ref rands 1))
				    vec
				    (make-Control-frame (+ ip 1)
							env-lex
							(vector-copy-prefix
							  env-tmp
							  (vector-ref rands 2))
							cont))]
		      [(return)
		       (continue cont)]
		      [(halt)
		       (make-DS-string "halt")]
		      [(nop)
		       (loop (+ ip 1) env-lex vec cont)]
		      [else
		       (error 'run
			      "Illegal instruction: ~s"
			      (vector-ref table-code-rator ip))]))))]
	     [application
	      (lambda (c vec cont)
		;;; c == procedure to apply
		;;; vec == vector of actual parameters
		;;; cont == control register
		(case-record c
		  [(DS-closure offset env-lex unique-tag)
		   (let ([arity (vector-ref table-lam-arity offset)])
		     (loop (vector-ref table-lam-entry offset)
			   (cons
			     (cond
			       [(= arity (vector-length vec))
				vec]
			       [(= arity -1)
				(vector (vector->DS-list vec))]
			       [else
				(error 'user-procedure-application
				       "arity mismatch for closure number ~s: ~s and ~s"
				       offset
				       arity
				       (vector->list vec))])
			     env-lex)
			   (vector)
			   cont))]
		  [(DS-primitive name arity p)
		   (begin
		     (check-arity name arity vec)
		     (case p
		       [(apply)
			(begin
			  (check-type 'apply vec 'procedure)
			  (check-DS-proper-list? (vector-ref vec 1))
			  (application (vector-ref vec 0)
				       (DS-list->vector (vector-ref vec 1))
				       cont))]
		       [(call/cc)
			(begin
			  (check-type 'call/cc vec 'procedure)
			  (application (vector-ref vec 0)
				       (vector (make-DS-continuation cont))
				       cont))]
		       [else
			(p vec (lambda ()
				 (continue cont)))]))]
		  [(DS-continuation cont)
		   (if (= 1 (vector-length vec))
		       (begin
			 (vector-set! aux-res 0 (vector-ref vec 0))
			 (continue cont))
		       (error 'continuation-application
			      "arity mismatch: ~s" (vector->list vec)))]
		  [else
		   (error 'application
			  "unapplicable value: ~s" c)]))]
	     [continue
	      (lambda (cont)
		(case-record cont
		  [(Control-frame ip env-lex tmp cont)
		   (begin
		     (vector-restore-prefix! env-tmp tmp)
		     (loop ip env-lex (vector) cont))]
		  [(Control-frame-initial)
		   (vector-ref aux-res 0)]
		  [else
		   (error 'application
			  "fudged continuation: ~s" cont)]))])
      (loop 0 '() (vector) (make-Control-frame-initial)))))


;;;;;;;;;;
;;; Initialization of the global tables,
;;; resolution of cross-references,
;;; and run:

(define run-program
  (lambda (p)
    (case-record p
      [(Compiled-Program number-of-global-definitions
			 number-of-temporaries
			 number-of-results
			 the-lambdas
			 the-code
			 the-tag)
       (begin
	 (warn "Initializing ")
	 (set! env-lib (vector-copy env-lib-reference))
	 (set! env-glo (make-vector number-of-global-definitions
				    DS-undefined))
	 (set! env-tmp (make-vector number-of-temporaries
				    DS-nil))
	 (set! aux-res (make-vector number-of-results
				    DS-nil))
	 (let ([number-of-lambdas (length the-lambdas)])
	   (begin
	     (set! table-lam-arity (make-vector number-of-lambdas 0))
	     (letrec ([walk
		       (lambda (n ls)
			 (if (= n number-of-lambdas)
			     'done
			     (begin
			       (vector-set! table-lam-arity n (caar ls))
			       (walk (+ n 1) (cdr ls)))))])
	       (walk 0 the-lambdas))))
	 (crossref the-code
		   (lambda (lookup-ref)
		     (begin
		       (set! table-lam-arity
			     (list->vector (map1 car the-lambdas)))
		       (set! table-lam-entry
			     (list->vector (map1 (lambda (a)
						   (lookup-ref (cadr a)))
						 the-lambdas)))
		       (warnl "/ initialized")
		       (run-instructions)))))]
      [else
       (error 'run-program "not a compiled program")])))

(define trace-vm 0)


;;;;;;;;;;

;;; end of "vm.scm"
