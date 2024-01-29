; We think of subsets of s
; as a combination of the first
; element of s
; and the subsets of the rest of s.
; This relation can be defined using
; reccurence (recursivity).
; let S , let K = {{e_1} U {k_i} for k_i in subsets(S - {e_1})}
; let N = cardinal( S)
; subsets(S) = subsets(S - {e_1}) U K
; K can be established as a map of every element k_i to
; (cons e_1 k_i).
; (subsets(S - {e_1}) U K can be established with appending
; subsets( S - {e_1}) to K.
; Naturally the call to subsets ( S - {e_1} ) would entail
; a call to subsets ( (S - {e_1}) - {e_2} ) and so on , until
; we reach a point where
; subsets((S - {e_1}) ... - {e_N}) = subsets(empty_set) = '()
; At that point we would have a chain of returns
; that would construct subsets( S - {e_i}) with the recurrence
; relation in place
(define (subsets s)
  (cond ((null? s) (list '()))
	(else
	 (let ((rest (subsets (cdr s))))
	   (append rest (map (lambda (e)
			       (if (not (null? (car s)))
				   (cons (car s) e)
				   '()))
			 rest)))))) 

(subsets (list 1 2 3))
