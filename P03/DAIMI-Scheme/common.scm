;;; DAIMI-Scheme/common.scm
;;; the DAIMI-Scheme compiler and VM
;;; Olivier Danvy <danvy@brics.dk>
;;; October 2002, October 2003

;;;;;;;;;;

(define the-tag "DAIMI-Scheme compiler, 03-10-22")

;;;;;;;;;;

(load-relative "lib/predefined.scm")

(load-relative "lib/gensym.scm")

(load-relative "lib/verbose.scm")

;;;;;;;;;;

(load-relative "lib/0parser.scm")

(load-relative "lib/1desugarer.scm")

(load-relative "lib/2boxer.scm")

(load-relative "lib/3linearizer.scm")

(load-relative "lib/4scoper.scm")

(load-relative "lib/5factorizer.scm")

(load-relative "lib/6codegen.scm")

(load-relative "lib/7dump.scm")

(load-relative "lib/vm.scm")

;;;;;;;;;;

(define verbose #t)

(define pretty #t)

;;;;;;;;;;

(define strip-Scheme-filename
  (lambda (filename)
    (cond
      [(not (string? filename))
       (error 'strip-Scheme-filename "not a file name: ~s" filename)]
      [(string-suffix? filename ".scm")
       (substring filename 0 (- (string-length filename) 4))]
      [else
       filename])))

(define normalize-Scheme-filename
  (lambda (filename)
    (if (string-suffix? filename ".scm")
	filename
	(string-append filename ".scm"))))

;;;;;;;;;;

(define asm
  (lambda (filename)
    (begin
      (reset-gensym!)
      (codegen-program
	(factorize-program
	  (scope-program
	    (linearize-program
	      (box-program
		(desugar-program
		  (parse-program
		    (read-file
		      (normalize-Scheme-filename
			filename))))))))))))

(define asmp
  (lambda (filename)
    (begin
      (case-record (asm filename)
	[(Compiled-Program number-of-global-definitions
			   number-of-temporaries
			   number-of-results
			   the-lambdas
			   the-code
			   the-tag)
	 (list 'DAIMI-SchemeE03
	       (list number-of-global-definitions
		     number-of-temporaries
		     number-of-results)
	       the-lambdas
	       the-code
	       the-tag)]
	[else
	 (error asmp "not a compiled program")]))))

(define dump
  (lambda (filename)
    (let ([filename (strip-Scheme-filename filename)])
      (let ([p (asm (string-append filename ".scm"))])
	(call-with-output-file (string-append filename ".dsa")
			       (lambda (port)
				 (dump-compiled-program p port)))))))

(define run
  (lambda (x)
    ((if pretty DS2S (lambda (a) a))
     (run-program (if (string? x)
		      (cond
			[(string-suffix? x ".scm")
			 (asm x)]
			[(string-suffix? x ".dsa")
			 (let ([dsa (call-with-input-file x read)])
			   (if (eqv? (car dsa) 'DAIMI-SchemeE03)
			       (let* ([rest (cdr dsa)]
				      [g (car rest)]
				      [rest (cdr rest)]
				      [the-lambdas (car rest)]
				      [rest (cdr rest)]
				      [the-code (car rest)]
				      [the-tag (car (cdr rest))])
				 (apply
				   (lambda (number-of-global-definitions
					     number-of-temporaries
					     number-of-results)
				     (make-Compiled-Program
				       number-of-global-definitions
				       number-of-temporaries
				       number-of-results
				       the-lambdas
				       the-code
				       the-tag))
				   g))
			       (error 'run "invalid header in ~s" x)))]
			[else
			 (asm (string-append x ".scm"))])
		      x)))))

; (define compile-program
;   (lambda (source-filename target-filename)
;     (let ([p (asm source-filename)])
;       (call-with-output-file
;         target-filename
;         (lambda (port)
;           (begin
;             (assemble-program p endian port)
;             'done))))))

;;;;;;;;;;

;;; end of "common.scm"
