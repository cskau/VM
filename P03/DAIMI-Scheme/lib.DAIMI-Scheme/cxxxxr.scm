;;; lib.DAIMI-Scheme/cxxxxr.scm
;;; 

;;; requires "cxxxr.scm"
;;; requires "compose.scm"

;;;;;;;;;;

(define cadddr (compose car cdddr))
(define cddddr (compose cdr cdddr))

;;;;;;;;;;

;;; end of "cxxxxr.scm"
