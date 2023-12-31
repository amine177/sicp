(define (tree-map fn tree)
  (map (lambda (node)
	 (if (pair? node)
	     (tree-map fn node)
	     (fn node)))
       tree))

(define (square-tree tree) (tree-map square tree))

(define tree (list 1 (list 2 (list 3 4) 5)
		   (list 6 7)))

(square-tree tree)
