(define (accumulate op initial sequence) 
   (if (null? sequence) 
       initial 
       (op (car sequence) 
           (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
  (accumulate (lambda (x y)
		(if (pair? x)
		    (+ y (count-leaves x))
		    (+ 1 y)))
	      0
	      t))

(define (count-leaves-map t)
  (accumulate +
	      0
	      (map (lambda (node)
		     (if (pair? node)
			 (count-leaves-map node)
			 1))
		   t)))



(define x (cons (list 1 2) (list 3 4)))
(count-leaves (list x x x))
(count-leaves (list 1 (list 2) x))
(count-leaves-map '(1 (list 2)))
(count-leaves '(1 (list 2)))
