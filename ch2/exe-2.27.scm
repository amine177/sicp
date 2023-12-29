(define (deep-reverse lst)
  (cond ((null? lst) '())
	((not (pair? lst)) (list lst))
	((= (length lst) 1) lst)
	(else
	 (append (deep-reverse (cdr lst))
		(deep-reverse (list (car lst)))))))
(deep-reverse (list 3 3 4))
