;;; DAIMI-Scheme.compiler/compiler.scm
;;; the DAIMI-Scheme compiler and VM, to be loaded in Petite Chez Scheme
;;; Olivier Danvy <danvy@brics.dk>
;;; October 2002

(case-sensitive #t)

;;;

(define extract-directory-name
  (lambda (filename)
    (if (string? filename)
	(let ([len (string-length filename)])
	  (letrec ([walk (lambda (offset)
			   (cond
			     [(negative? offset)
			      ""]
			     [(char=? (string-ref filename offset) #\/)
			      (substring filename 0 (+ offset 1))]
			     [else
			      (walk (- offset 1))]))])
	    (walk (- len 1))))
	(error 'extract-directory-name "not a file name: ~s" filename))))

(define *relativity* "")

(define load-relative
  (lambda (filename)
    (let ([directoryname (extract-directory-name filename)])
      (cond
	[(eqv? directoryname "")
	 (load (string-append *relativity* filename))]
	[(char=? (string-ref directoryname 0) #\/)
	 (fluid-let ([*relativity* directoryname])
           (load filename))]
	[else
	 (fluid-let ([*relativity* (string-append *relativity* directoryname)])
           (load filename))]))))

;;;

(load-relative "lib/standard-prelude.scm")

;;;

(load-relative "common.scm")

(load-relative "lib/unparser.scm")

(load-relative "lib/ditcher.scm")

;;;

(define des
  (lambda (filename)
    (begin
      (reset-gensym!)
      (unparse-program
	(desugar-program
	  (parse-program
	    (read-file
	      (normalize-Scheme-filename
		filename))))))))

(define box
  (lambda (filename)
    (begin
      (reset-gensym!)
      (unparse-program
	(box-program
	  (desugar-program
	    (parse-program
	      (read-file
		(normalize-Scheme-filename
		  filename)))))))))

(define lin
  (lambda (filename)
    (begin
      (reset-gensym!)
      (linearize-program
	  (box-program
	    (desugar-program
	      (parse-program
		(read-file
		  (normalize-Scheme-filename
		    filename)))))))))

(define ditch
  (lambda (filename)
    (begin
      (reset-gensym!)
      (ditch-program
	(linearize-program
	  (box-program
	    (desugar-program
	      (parse-program
		(read-file
		  (normalize-Scheme-filename
		    filename))))))))))

(define scope
  (lambda (filename)
    (begin
      (reset-gensym!)
      (scope-program
	(linearize-program
	  (box-program
	    (desugar-program
	      (parse-program
		(read-file
		  (normalize-Scheme-filename
		    filename))))))))))

(define factorize
  (lambda (filename)
    (begin
      (reset-gensym!)
      (factorize-program
	(scope-program
	  (linearize-program
	    (box-program
	      (desugar-program
		(parse-program
		  (read-file
		    (normalize-Scheme-filename
		      filename)))))))))))

;;;

(define compile-program!
  (lambda (source-filename target-filename pretty)
    (let ([p (ditch source-filename)])
      (begin
	(if (file-exists? target-filename)
	    (delete-file target-filename)
	    'ikke-noget)
	(call-with-output-file
	  target-filename
	  (lambda (port)
	    (begin
	      (if (and (pair? p)
		       (eqv? (car p) 'begin))
		  (begin
		    (display "(begin " port)
		    (newline port)
		    (newline port)
		    (if pretty
			(for-each1 (lambda (e)
				     (begin
				       (pretty-print e port)
				       (newline port)))
				   (cdr p))
			(for-each1 (lambda (e)
				     (begin
				       (write e port)
				       (newline port)
				       (newline port)))
				   (cdr p)))
		    (display ")" port)
		    (newline port))
		  (if pretty
		      (pretty-print p port)
		      (write p port)))
	      'done)))))))

(define source-compile-program
  (lambda (filename)
    (let ([filename (strip-Scheme-filename filename)])
      (compile-program! filename
			(string-append filename ".scm-lin")
			#f))))

(define source-compile-program-pretty
  (lambda (filename)
    (let ([filename (strip-Scheme-filename filename)])
      (compile-program! filename
			(string-append filename ".scm-lin")
			#t))))


;;; 

;;; end of "compiler.scm"
