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
  (= (exponant-modulus (random (- n 1)) (- n 1) n) 
     1))

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




(fermat-test 7)
