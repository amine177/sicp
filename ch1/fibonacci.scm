(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1))
		 (fib (- n 2))))))


(define (fib-t n)
  (fib-iter 1 0 n))

(define (fib-iter a b i)
  (if (= i 0)
      b
      (fib-iter (+ a b) a (- i 1))))

(fib 0)
(fib 1)
(fib 2)
(fib-t 0)
