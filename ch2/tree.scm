(define tree1 (cons (list 1 2) (list 3 4)))

(define (leave? e)
  (not (pair? e)))

(define (count-leaves tree)
  (newline)
  (display "treating the tree element ")
  (display tree)
  (newline)
  (cond ((null? tree) 0)
	((leave? tree) 1)
	(else (+
	       (count-leaves (cdr tree))
	       (count-leaves (car tree))))))

(count-leaves tree1)
