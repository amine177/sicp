(define (square-tree tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (square tree))
	(else
	 (cons (square-tree (car tree))
	       (square-tree (cdr tree))))))

(define (square-tree-map tree)
  (map (lambda (node)
	 (if (pair? node)
	     (square-tree-map node)
	     (square node)))
       tree))

(define tree (list 1 (list 2 (list 3 4) 5)
		   (list 6 7)))

(square-tree tree)
(square-tree-map tree)
