 
(define (even x)
  (= (remainder x 2)  0))


(define (fast-exponentiation-iterative b n)
  (define (fa-e-iter b acc_even acc_odd n)
    (cond
     ((= n 0)
      acc_odd)
     ((even n)
      (fa-e-iter b (* acc_even acc_even) acc_odd (/ n 2)))
     (else
      (fa-e-iter b acc_even (* acc_odd acc_even)  (- n 1)))))
  (trace fa-e-iter)
  (fa-e-iter b b 1 n))


(fast-exponentiation-iterative 4 2)
