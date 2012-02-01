;;; DAIMI-Scheme/lib/predefined.scm
;;; names of the predefined procedures in DAIMI-Scheme
;;; Olivier Danvy <danvy@brics.dk>
;;; October 2002, October 2003


(define predefined-procedures
  '(integer? + - * quotient remainder < <= = >= >
    boolean?
    symbol?
    char? char->integer integer->char
    string make-string string? string-length string-append string=? string-ref
    string->symbol symbol->string
    pair? cons car cdr set-car! set-cdr! null?
    vector make-vector vector? vector-length vector-ref vector-set!
    procedure? apply
    eqv?
    call/cc exit
    open-input-file
    input-port? close-input-port current-input-port
    read-char peek-char
    eof-object?
    open-output-file
    output-port? close-output-port current-output-port
    write-char
    ))


;;; end of "predefined.scm"
