(define (pascal n k)
  (cond ((= n 1) 1)
	((= n k) 1)
	(else (+ (pascal (- n 1) k) (pascal (- n 1) (- k 1))))))


(pascal 4 2)



      
