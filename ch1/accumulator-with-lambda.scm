(define (accumulator combiner term next filter null-value a b)
  (if (> a b)
      null-value
      (if (filter a)
	  (combiner (term a)
		(accumulator
		 combiner
		 term
		 next
		 filter
		 null-value
		 (next a)
		 b))
	  (combiner null-value
		    (accumulator
		     combiner
		     term
		     next
		     filter
		     null-value
		     (next a)
		     b)))))



(define (sum combiner term next filter null-value a b)
  (accumulator combiner term next filter null-value a b))


(define (pi-sum a b)
  (sum + ; combiner
       (lambda (x) (/ 1.0 (* x (+ x 2)))) ; term
       (lambda (x) (+ x 4))
       (lambda (x) true)
       0
       a
       b))

(define (pi n)
  (* 2
     (accumulator
      *
      (lambda (x) (* (/ x (- x 1)) (/ x (+ x 1))))
      (lambda (x) (+ x 2))
      (lambda (x) true)
      1
      2.0
      n)))


(pi-sum 1 10)
(pi 10000.0)
		    
