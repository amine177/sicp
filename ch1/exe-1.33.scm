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

(define (exponant a n)
  (cond ((= n 0) 1)
	((even? n)
	 (square (exponant a (/ n 2))))
	(else (* a (exponant a (- n 1))))))
	
(define (fermat-test n)
  (if (= n 1) true
  (= (exponant-modulus (+ 1 (random (- n 1))) (- n 1) n) 
     1)))

(define (square-mod-miller-check x n)
  (cond ((and
	  (= (remainder (square x) n) 1)
	  (not (= x 1))
	  (not (= x (- n 1))) 0))
	 (else (remainder (square x) n))))

(define (exponant-modulus x k n) ; x^k mod n = x^(k/2)^2mod n

  (cond ((= k 0) 1)
	((even? k)
	 (square-mod-miller-check (exponant-modulus x (/ k 2) n) n))
	(else
	 (remainder (* x (exponant-modulus x (- k 1) n)) n))))


(define (sum-square-prime a b)
  (define (next x)
    (+ x 1))
  (define (term x)
    (square x))
  (accumulate + term next 0 fermat-test a b))

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
