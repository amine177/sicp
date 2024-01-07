(define (f-left op initial sequence)
  (if (null? sequence)
      initial
      	    (f-left op
			(op initial (car sequence))
			(cdr sequence))))
		 
(define (fold-left-iter op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest)) (cdr rest))))
  (iter initial sequence))


(fold-left-iter / 1 (list 1 2 3)); (fold-left / 1 (list 1 2 3))
				; (fold-left / (/ 1 1) '(2 3))
				; (fold-left / 1 '(2 3))
				; (fold-left / (/ 1 2) '(3))
				; (fold-left / 1/2 '(3))
				; (fold-left (/ 1/2 3) '())
				; (fold-left 1/6 '())
				; 1/6

(fold-right / 1 (list 1 2 3)) ; (fold-right / 1 (list 1 2 3))
			      ; (/ 1 (fold-right / 1 '(2 3)))
			      ; (/ 1 (/ 2 (fold-right / 1 '(3))))
			      ; (/ 1 (/ 2 (/ 3 (fold-right / 1 '()))))
			      ; (/ 1 (/ 2 (/ 3 (/ 1))))
			      ; 3/2

(fold-right
 list
 nil
 (list 1 2 3)) ; (list 1 (fold-right list nil '(2 3)))
               ; (list 1 (list 2 (fold-right list nil '(3))))
               ; (list 1 (list 2 (list 3 (fold-right list nil '()))))
               ; (list 1 (list 2 (list 3 '())))

(fold-left
 list
 nil
 (list 1 2 3)) ; (fold-left list (list '() 1) '(2 3))
               ; (fold-left list (list (list '() 1) 2) '(3))
               ; (fold-left list (list (list (list '() 1) 2) 3) '())
               ; (list (list (list '() 1) 2) 3)

; OP should be commutative


