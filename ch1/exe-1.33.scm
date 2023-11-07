(define (accumulate combiner term next null-value filter a b)
  (cond ((> a b)
      null-value)
	((filter a)
	 (combiner (term a)
		   (accumulate
		    combiner
		    term
		    next
		    null-value
		    filter
		    (next a)
		    b)))
	(else (combiner null-value
		(accumulate
		 combiner
		 term
		 next
		 null-value
		 filter
		 (next a)
		 b)))))


(define (square x)
  (* x x))

(define (even? x)
  (= (bitwise-and x 1) 0))

(define (even x)
  (= (bitwise-and x 1) 0))
(define (square x)
  (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n d)
  (cond ((> (square d) n) n)
	((= (remainder n d) 0) d)
	(else (find-divisor n (+ d 1)))))

(define (prime? n)
  (= (smallest-divisor n) n))


(define (sum-square-prime a b)
  (define (next x)
    (+ x 1))
  (define (term x)
    (square x))
 (accumulate + term next 0 prime? a b))

(sum-square-prime 1 5)
