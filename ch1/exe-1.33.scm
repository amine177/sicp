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

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (product-positive-relative-prime n)
  (define (next x)
    (+ x 1))
  (define (term x)
    x)
  (define (less-relative-prime-n x)
    (and (= (gcd x n) 1) (< x n)))
  (accumulate *
	      term
	      next
	      1
	      less-relative-prime-n
	      1
	      n))

(sum-square-prime 1 5)
(product-positive-relative-prime 6)
