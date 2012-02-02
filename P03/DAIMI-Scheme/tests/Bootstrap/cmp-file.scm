;;; tests/Bootstrap/cmp-file.scm
;;; Prompts for a file name, reads it, and compiles the program in that file.

(load-relative "../../lib.DAIMI-Scheme/standard-prelude.scm")
(load-relative "../../common.scm")

(define prompt-and-read
  (lambda (s)
    (begin
      (display s)
      (read (current-input-port)))))

(let ([name (prompt-and-read "file name? ")])
  (dump (if (string? name)
            name
            (symbol->string name))))
