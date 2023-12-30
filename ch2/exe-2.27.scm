(define (deep-reverse lst)
  (cond ((null? lst) '())
	((not (pair? (car lst)))
	 (append (deep-reverse (cdr lst))
		 (list (car lst))))
	(else
	 (append (deep-reverse (cdr lst))
		(list (deep-reverse (car lst)))))))
(deep-reverse (list 3 (list 1 (list 5 6) 3) 4))

(define (reverse lst)
  (cond ((null? lst) '())
	(else
	 (append (reverse (cdr lst))
		 (list (car lst))))))

(define (elegant-deep-reverse lst)
  (cond ((pair? lst)
	 (reverse (map elegant-deep-reverse lst)))
	(else
	 lst)))
(elegant-deep-reverse (list 3 (list 1 (list 5 6) 3) 4))
