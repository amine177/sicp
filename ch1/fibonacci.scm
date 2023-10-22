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


(define (fib_it n)
  (fib_iter 0 1  n))

(define (fib_iter  u_n u_next counter)
  (if (= counter 0) u_n
      (fib_iter u_next (+ u_n u_next)  (- counter 1)))) ; this should
					; be more succint than the
					; book. It makes it clear
					; that the U_N = U_Next
					; and U_next = U_N + U_next
(fib_it 6)
(fib 6)
