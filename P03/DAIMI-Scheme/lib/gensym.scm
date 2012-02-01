;;; DAIMI-Scheme.compiler/lib/gensym.scm
;;; generator of fresh symbols
;;; Olivier Danvy <danvy@brics.dk>
;;; October 2002

;;; (gensym! s)
;;; Generates a symbol out of a string by catenating a number to that string.
;;; Unsafe: there are no guarantee that the symbol does not exist already.

;;; (reset-gensym!)
;;; Resets the counter to -1.

;;;;;;;;;;

(define gensym-count -1)

(define reset-gensym!
  (lambda ()
    (set! gensym-count -1)))

(define gensym!
  (lambda (str)
    (begin
      (set! gensym-count (+ gensym-count 1))
      (string->symbol (string-append str (integer->string gensym-count))))))

;;;;;;;;;;

;;; end of "gensym.scm"
