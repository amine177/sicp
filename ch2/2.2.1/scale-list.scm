(define nil '())
(define (scale-list lst scale)
  (cond ((null? lst)
	 nil)
	(else
	 (cons (* (car lst) scale)
	       (scale-list (cdr lst) scale)))))

(scale-list (list 1 2 3 4) 11)
(map (lambda (x) (* x 10)) (list 1 2 3 4))
