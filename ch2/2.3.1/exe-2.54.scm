(define (equal? x y)
  (cond ((and (null? x) (null? y))
	 #t)
	((and (not (pair? x)) (not (pair? y)))
	 (eq? x y))
	(else
	 (and (equal? (car x) (car y)) (equal? (cdr x) (cdr y))))))
