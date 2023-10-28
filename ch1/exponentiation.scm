(define (e b n)
  (if (= n 0)
      1
      (* b (e  b (- n 1)))))

(e 2 10)


(define (e-iter b n)
  (define (e-iter-internal total x p)
    (if (= p 0)
	total
	(e-iter-internal (* total x) x (- p 1))))
  (e-iter-internal 1 b n))

(e-iter 2 10)

(define (even x)
  (= (remainder x 2)  0))

(define (fast-exp b n)
  (if (= n 0)
      1
      (if (even n)
	  (* (fast-exp b (/ n 2)) (fast-exp b (/ n 2)))
	   (* b (fast-exp b (- n 1))))))

(fast-exp 2 10)
