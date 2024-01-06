(define (sum-sqr-odd tree)
  (cond ((null? tree) 0)
	((not (pair? tree))
	 (cond ((odd? tree) (* tree tree))
	       (else 0)))
	((pair? tree)
	 (+ (sum-sqr-odd (car tree))
	    (sum-sqr-odd (cdr tree))))))

(sum-sqr-odd (list 1 (list 2 3)))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (list tree))
	(else (append (enumerate-tree (car tree))
		      (enumerate-tree (cdr tree))))))
	      
(define (s-s-o tree)
  (reduce + 0 (map (lambda (x) (* x x)) (filter odd? (enumerate-tree tree)))))

