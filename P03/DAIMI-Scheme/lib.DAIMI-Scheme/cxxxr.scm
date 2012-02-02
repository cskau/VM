;;; lib.DAIMI-Scheme/cxxxr.scm
;;; 

;;; requires "cxxr.scm"
;;; requires "compose.scm"

;;;;;;;;;;

(define caaar (compose car caar))
(define cdaar (compose cdr caar))
(define cadar (compose car cdar))
(define cddar (compose cdr cdar))
(define caadr (compose car cadr))
(define cdadr (compose cdr cadr))
(define caddr (compose car cddr))
(define cdddr (compose cdr cddr))

;;;;;;;;;;

;;; end of "cxxxr.scm"
