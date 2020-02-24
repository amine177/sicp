(define (fast-exp b n)
  (cond ((= n 0) 1)
	((iseven n)
	 (* (fast-exp b (/ n 2)) (fast-exp b (/ n 2))))
	(else (* b (fast-exp b (- n 1))))))

(define (iseven n)
  (= (remainder n 2) 0))
(fast-exp 3 2)
