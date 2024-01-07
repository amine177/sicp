(define (reverse-r sequence)
  (fold-right (lambda (x y)
		(if (null? y)
		    (cons x '())
		    (cons y x)))
	      '()
	      sequence))

(define (reverse-l sequence)
  (fold-left (lambda (x y)
		(if (null? y)
		    (cons x y)
		    (cons y x)))
	      '()
	      sequence))

(define (reverse-reduce seq)
  (reduce (lambda (x y)
	    (if (not (pair? y))
		(cons x (cons y '()))
		(cons x y))) '() seq))


(reverse-r (list 1 2 3 4))
(reverse-l (list 1 2 3))
