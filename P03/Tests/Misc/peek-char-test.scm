(define list
  (lambda xs
    xs))

(list (peek-char (current-input-port))
      (peek-char (current-input-port))
      (peek-char (current-input-port)))

;;; expected result: a list of 3 times the first character that
;;; will be typed in stdin
