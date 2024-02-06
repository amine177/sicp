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

(define (flatmap fn sequence)
  (reduce append '() (map fn sequence)))

(define (tree->list-map tree)
  (cond
   ((null? tree) '())
   ((not (pair? tree)) (list tree))
   (else
    (flatmap (map (lambda (x)
	   (tree->list-map x)) tree)))))
	       

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elements n)
  (cond ((= n 0)
	 (cons '() elements))
	(else
	 (let ((left-size (quotient (- n 1) 2)))
	   (let ((left-result (partial-tree elements left-size)))
	     (let ((left-tree (car left-result))
		   (non-left-elements (cdr left-result)))
	       (let ((parent-node (car non-left-elements))
		     (right-result (partial-tree
				    (cdr non-left-elements)
				    (- n left-size 1))))
		 (let ((right-tree (car right-result))
		       (remaining-elements (cdr right-result)))
		   (cons (make-tree parent-node
				    left-tree
				    right-tree)
			 remaining-elements)))))))))



(define (union-set-list s1 s2)
  (cond	 ((null? s2) s1)
	 ((null? s1) s2)
	 ((< (car s1) (car s2))
	       (cons
		(car s1)
		(union-set-list (cdr s1)
			   s2)))
	      ((> (car s1) (car s2))
	       (cons (car s2) (union-set-list
			       s1
			       (cdr s2))))
	      ((= (car s1) (car s2))
	       (cons (car s1) (union-set-list
			       (cdr s1)
			       (cdr s2))))))
; O(n)
(define (union-set s1 s2)
  (let ((list-set1 (tree->list-2 s1)) ; O(n)
	(list-set2 (tree->list-2 s2))); O(n)
    (list->tree (union-set-list list-set1 list-set2)))) ; O(n)

(union-set (list->tree '( 2 )) (list->tree '(1 2 3))) 
