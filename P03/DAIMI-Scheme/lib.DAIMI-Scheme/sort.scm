;;; lib.DAIMI-Scheme/sort.scm
;;; 

;;; requires filter.scm

;;;;;;;;;;

(define merge
  (lambda (p xs ys)
    (cond
      [(null? xs)
       ys]
      [(null? ys)
       xs]
      [(p (car xs) (car ys))
       (cons (car xs) (merge p (cdr xs) ys))]
      [else
       (cons (car ys) (merge p xs (cdr ys)))])))

(define sort
  (lambda (p xs)
    (if (or (null? xs) (null? (cdr xs)))
	xs
	(let* ([x1 (car xs)]
	       [q (lambda (x)
		    (p x1 x))])
	  (merge p
		 (list x1)
		 (merge p
			(sort p (filter-in q (cdr xs)))
			(sort p (filter-out q (cdr xs)))))))))

;;;;;;;;;;

;;; end of "sort.scm"
