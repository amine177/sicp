(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
	      (cons (entry tree)
		    (tree->list-1
		     (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
	result-list
	(copy-to-list (left-branch tree)
		      (cons (entry tree)
			    (copy-to-list
			     (right-branch tree)
			     result-list)))))
  (copy-to-list tree '()))

(tree->list-1  '(7
		 (3 (4 (5 () ()) (11 () ())) (14 (22 () ()) (15 () ()))) (1 () ())))
(tree->list-2  '(7
		 (3 (4 (5 () ()) (11 () ())) (14 (22 () ()) (15 () ()))) (1 () ())))
					
; a. Yes they produce the same result for every tree.
;    (1 3 5 7 9 11) in all cases
; b. tree->list-1 : O(n log(n))
;    tree->list-2 : O(n)




