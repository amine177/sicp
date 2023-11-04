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

(define (timed-prime-test n)
  (start-prime-test n (runtime)))


(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))
      false))
(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time)
  true)

(define (find-smallest-three-primes counter lower-limit upper-limit)
  (cond ((and (<= lower-limit upper-limit) (< counter 3))
	 (if (timed-prime-test lower-limit)
	     (find-smallest-three-primes  (+ counter 1)
					  (if (even lower-limit)
					      (+ lower-limit 1)
					      (+ lower-limit 2))
					  upper-limit)
	     (find-smallest-three-primes  counter
					  (if (even lower-limit)
					      (+ lower-limit 1)
					      (+ lower-limit 2))
					  upper-limit)))))

(find-smallest-three-primes 0 1000 10000)
;; 1009 *** 9.999999999999787e-3
;; 1013 *** 1.0000000000000231e-2
;; 1019 *** 1.0000000000000231e-2


(find-smallest-three-primes 0 10001 100000)
;; 10007 *** 2.0000000000000018e-2
;; 10009 *** 9.999999999999787e-3
;; 10037 *** 2.0000000000000018e-2


(find-smallest-three-primes 0 100001 1000000)
;; 100003 *** 2.0000000000000018e-2
;; 100019 *** 2.0000000000000018e-2
;; 100043 *** 2.0000000000000018e-2


(find-smallest-three-primes 0 1000001 10000000)
;; 1000003 *** 2.0000000000000018e-2
;; 1000033 *** 2.0000000000000018e-2
;; 1000037 *** 2.0000000000000018e-2


(find-smallest-three-primes 0 10000001 100000000)
;; 10000019 *** 2.9999999999999805e-2
;; 10000079 *** .03000000000000025
;; 10000103 *** 2.9999999999999805e-2


(find-smallest-three-primes 0 100000001 1000000000)
;; 100000007 *** .03000000000000025
;; 100000037 *** 2.9999999999999805e-2
;; 100000039 *** .03000000000000025
