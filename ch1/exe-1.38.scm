(define (continued-fraction n d k)
  (define (cont-frac i)
    (cond ((= i k)
	 (/ (n k) (d k)))
	(else
	 (/ (n i) (+ (d i) (cont-frac (+ i 1)))))))
  (cont-frac 1.0))

(define (continued-fraction-iterative n d k)
  (define (cont-frac-iter i total)
    (cond ((= i 0)
	 total)
	(else
	 (cont-frac-iter (- i 1) (/ (n i) (+ (d i) total))))))
  (cont-frac-iter k (/ (n k) (d k))))


(continued-fraction
	 (lambda (x) 1.0)
	 (lambda (x)
		 (cond
		 ((= (remainder x 3) 2)
		  (* 2 (/ (+ x 1) 3)))
		 (else 1)))
	     100)



