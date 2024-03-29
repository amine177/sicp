; a. partial-tree constructs a tree by
; gluing together the left tree
; and right tree to the head of the current list
; every side is computed by recursively 
; constructing the subtree from 
; leftmost of side in the same fashion
; then doing the same for the rightmost side 
; the rightmost side is what was left after
; removing the root node and  the left subtree elemnts
; from the list
;
;
;       5
;      1   9
;     3   7 11
; b. O(n)   
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(trace partial-tree)
(define (partial-tree elts n)
  (cond  ((= n 0) '())
	 (else
	  (let ((left-size (quotient (- n 1) 2)))
	    (let ((left-result
		   (partial-tree elts left-size)))
	      (let ((left-tree (car left-result))
		    (non-left-elts (cdr left-result))
		    (right-size (- n (+ left-size 1))))
		(let ((this-entry (car non-left-elts))
		      (right-result
		       (partial-tree
			(cdr non-left-elts)
			right-size)))
		  (let ((right-tree (car right-result))
			(remaining-elts
			 (cdr right-result)))
		    (cons (make-tree this-entry
				     left-tree
				     right-tree)
			  remaining-elts)))))))))
