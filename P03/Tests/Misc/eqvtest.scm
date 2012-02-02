;;; eqvtest.scm
;;; Tests the behaviour of the eqv? procedure.
;;; Aske Simon Christensen <aske@brics.dk>, November 22, 2002

; The rows of the output indicate the type of the compared values.
; The columns indicate what is compared:
;   Same: A value is compared to itself.
;   Iden: Two identical constants are compared
;   Comp: A constant is compared to the result of an expression
;         that evaluates to the same value.
;   Proc: The results of two calls of the same procedure returning
;         a constant are compared.
;   Diff: Two constants with different values are compared.
;
; The eqv? procedure in DAIMI-Scheme must follow the specification for eqv?
; in the R5RS, that is, the results must obey the following:
;
; - The results in the first column must all be yes.
; - The results in the last column must all be no.
; - The results in the middle three entries in the rows for integers,
;   characters, booleans, symbols and empty lists must all be yes.
;
; All other results are implementation dependent.
;
; Note that this program does not test comparison of values of different types.
; Such a comparison must always return false.

(define print
  (lambda (s)
    (let ([len (string-length s)])
      (letrec ([p (lambda (i)
                    (if (= i len)
		      'done
		      (begin
			(write-char (string-ref s i)
			  (current-output-port))
			(p (+ i 1)))))])
        (p 0)))))
(define newline
  (lambda (port)
    (write-char #\newline port)))
(define answer
  (lambda (x)
    (if x "yes  " "no   ")))
(define test
  (lambda (h a b c d f)
    (begin
      (print h)
      (print (answer (eqv? a a)))
      (print (answer (eqv? a b))) 
      (print (answer (eqv? a c)))
      (print (answer (eqv? (f) (f))))
      (print (answer (eqv? a d)))
      (newline (current-output-port)))))
(define makevec
  (lambda ()
    (let ([v (make-vector 3 1)])
      (begin
	(vector-set! v 1 2)
	(vector-set! v 2 3)
	v))))
(define makepair
  (lambda ()
    (let ([p '(1)])
      (begin
	(set-cdr! p 2)
	p))))
(define makelam
  (lambda ()
    (lambda () 42)))
(begin
  (newline (current-output-port))
  (print "Type       Same Iden Comp Proc Diff")
  (newline (current-output-port))
  (test "Integer    " 42             42             (+ 21 21)            36           (lambda () 42))
  (test "Character  " #\*            #\*            (integer->char 42)   #\$          (lambda () #\*))
  (test "Boolean    " #t             #t             (= 42 42)            #f           (lambda () #t))
  (test "String     " "*"            "*"            (string #\*)         "$"          (lambda () "*"))
  (test "(empty)    " ""             ""             (string)             "$"          (lambda () ""))
  (test "Symbol     " 'a             'a             (string->symbol "a") 'b           (lambda () 'a))
  (test "List       " '(1 2 3)       '(1 2 3)       (cons 1 '(2 3))      '(4)         (lambda () '(1 2 3)))
  (test "(empty)    " '()            '()            (cdr '(1))           '(4)         (lambda () '()))
  (test "Pair       " (cons 1 2)     (cons 1 2)     (makepair)           (cons 4 4)   (lambda () (cons 1 2)))
  (test "Vector     " (vector 1 2 3) (vector 1 2 3) (makevec)            (vector 4)   (lambda () (vector 1 2 3)))
  (test "(empty)    " (vector)       (vector)       (make-vector 0 'bla) (vector 4)   (lambda () (vector)))
  (test "Procedure  " (lambda () 42) (lambda () 42) (makelam)            (lambda x x) (lambda () (lambda () 42)))
)
