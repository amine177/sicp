; The squared list will be reversed.
; This is because we are appending
; the square of the current element
; at the start of the answer list.
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons (square (car things))
		    answer))))
  (iter items '()))

; The squared list will be a
; a list of cons pairs.
; This is because the first pair (nil (square first-element)) breaks
; the pointer and box notation of a list

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons answer
	       (square (car things))))))
  (iter items '()))


; This is the correct implementation.
; The currently calculated value should be
; appended at the end of the accumulator result.
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (append
	       answer
	       (list
		(square (car things)))))))
    (iter items '()))
