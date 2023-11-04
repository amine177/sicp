(define (change-combinations n) (count-change n 5))
(define (count-change amount kinds-of-coins)
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kinds-of-coins 0)) 0)
	(else (+ (count-change amount 
			       (- kinds-of-coins 1))
		 (count-change (- amount (first-denom
					  kinds-of-coins))
			       kinds-of-coins)))))

(define (first-denom kc)
  (cond ((= kc 1) 1)
	((= kc 2) 5)
	((= kc 3) 10)
	((= kc 4) 25)
	((= kc 5) 50)))

(change-combinations 100)
