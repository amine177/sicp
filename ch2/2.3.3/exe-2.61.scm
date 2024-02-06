(define (adjoin-set x s1)
  (display "\ns1: ")
  (display s1)
  (cond ((null? s1) (cons x '()))
	((> (car s1) x)
	 (cons x s1))
	(else (cons
	       (car s1)
	       (adjoin-set x (cdr s1))))))

(define (element-of-set? x s1)
  (cond ((null? s1) #f)
	((> (car s1) x) #f)
	((= (car s1) x) #t)
	(else (element-of-set? x (cdr s1)))))
	
