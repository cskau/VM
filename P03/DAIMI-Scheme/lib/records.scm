;;; DAIMI-Scheme.compiler/lib/records.scm
;;; syntactic extensions for simple records
;;; Olivier Danvy <danvy@brics.dk>
;;; October 2002, October 2003

(define-syntax define-record
  (lambda (expr)
    (define construct-name
      (lambda (template-identifier . args)
	(datum->syntax-object
	  template-identifier
	  (string->symbol
	    (apply string-append
		   (map (lambda (x)
			  (if (string? x)
			      x
			      (symbol->string (syntax-object->datum x))))
			args))))))
    (syntax-case expr ()
      [(define-record (name id1 ...))
       (with-syntax ([constructor (construct-name (syntax name)
						  "make-"
						  (syntax name))]
		     [predicate (construct-name (syntax name)
						"is-"
						(syntax name)
						"?")]
		     [record-length (+ (length (syntax (id1 ...))) 1)]
		     [(fresh ...)
		      (map (lambda (x)
			     (construct-name x (gensym)))
			   (syntax (id1 ...)))])
	 (syntax (begin
		   (define constructor
		     (lambda (fresh ...)
		       (vector 'name fresh ...)))
		   (define predicate
		     (lambda (x)
		       (and (vector? x)
			    (= (vector-length x) record-length)
			    (eqv? (vector-ref x 0) 'name)))))))])))


(define-syntax real-case-record
  (lambda (expr)
    (define construct-name
      (lambda (template-identifier . args)
	(datum->syntax-object
	  template-identifier
	  (string->symbol
	    (apply string-append
		   (map (lambda (x)
			  (if (string? x)
			      x
			      (symbol->string (syntax-object->datum x))))
			args))))))
    (syntax-case expr (else)
      [(real-case-record var)
       (syntax (error 'case-record "no matching clause"))]
      [(real-case-record var [else clause])
       (syntax clause)]
      [(real-case-record var ((name component ...) exp) clause ...)
       (with-syntax ([predicate (construct-name (syntax name)
						"is-"
						(syntax name)
						"?")])
         (syntax (if (predicate var)
		     (apply (lambda (component ...)
			      exp)
			    (cdr (vector->list var)))
		     (real-case-record var clause ...))))])))

(define-syntax case-record
  (lambda (expr)
    (syntax-case expr (else)
      [(case-record test clause ...)
       (not (identifier? (syntax test)))
       (syntax (let ([var test])
		 (real-case-record var clause ...)))]
      [(case-record test clause ...)
       (syntax (real-case-record test clause ...))])))

;;; end of records.scm
