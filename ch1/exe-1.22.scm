(define (even x)
  (= (bitwise-and x 1) 0))
(define (square x)
  (* x x))

(define (e-m a pwr n)
  (cond
   ((= pwr 0) 1)
   ((even pwr) (remainder (square (e-m a (/ pwr 2) n)) n))
   (else (remainder (* a (e-m a (- pwr 1) n)) n))))


(define (fermat-test n a  treshold)
  (cond
   ((= treshold 0) true)
   ((= (e-m a n n) a) (fermat-test n (+ 1  (random (- n 1)))
				   (- treshold 1)))
   (else false)))

(define (prime? n)
  (cond ((= n 2) true)
	(else
  (fermat-test n 2 1000))))

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


(find-smallest-three-primes 0 (square (square 1000000001))
			    (square (square 10000000000)))
;; 1000000004000000006000000004000000007 *** .17999999999999972
;; 1000000004000000006000000004000000251 *** .17999999999999972
;; 1000000004000000006000000004000000341 *** .16999999999999993
