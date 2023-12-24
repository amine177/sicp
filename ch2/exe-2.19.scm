(define (count-change amount kinds-of-coins)
  (cond ((= amount 0) 1)
	((or (< amount 0) (= (length kinds-of-coins) 0)) 0)
	(else (+ (count-change amount 
			       (cdr kinds-of-coins))
		 (count-change (- amount (car
					  kinds-of-coins))
			       kinds-of-coins)))))
(define (change-combinations n coins-list)
  (count-change n coins-list))

(change-combinations 100 (list 50 25 10 5 1))
(change-combinations 100 (list 1 5 10 25 50))
(change-combinations 100 (list 100 50 20 10 5 2 1 0.5))

;the procedure following the book
(define (no-more? coins)
  (= (length coins) 0))

(define (except-first-denomination coins)
  (cdr coins))

(define (first-denomination coins)
  (car coins))

(define (cc amount coins)
  (count-change-book amount coins))
(define (count-change-book amount coin-values)
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more? coin-values)) 0)
	(else (+ (count-change-book amount
			       (except-first-denomination
				coin-values))
		 (count-change-book (- amount (first-denomination
					  coin-values))
			       coin-values)))))

(cc 100 (list 50 25 10 5 1))
(cc 100 (list 1 5 10 25 50))
