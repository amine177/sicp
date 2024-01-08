(define (accumulate op initial sequence) 
   (if (null? sequence) 
       initial 
       (op (car sequence) 
           (accumulate op initial (cdr sequence)))))

(define (horner-eval x coeffiecient-sequence)
  (accumulate (lambda (e rest)
		(if (null? e)
		    rest
		    (+ e (* x rest))))
	      0
	      coeffiecient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))
