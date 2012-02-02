;;; lib.DAIMI-Scheme/compose.scm
;;; basic function composition

;;;;;;;;;;

(define compose
  (lambda (f g)
    ;;; [B -> C] * [A -> B] -> A -> C
    (lambda (x)
      (f (g x)))))

(define compose2
  (lambda (f g)
    ;;; [B -> C] * [A1 * A2 -> B] -> A1 * A2 -> C
    (lambda (x y)
      (f (g x y)))))

;;;;;;;;;;

;;; end of "compose.scm"
