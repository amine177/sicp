(define (fast-exp b n)

  (define (fast-exp-iter b acc n)
    (display n)
    (display "\n")
    (cond ((= n 0) 1)
	 ((= n 1) acc)
	 ((even n)
	  (fast-exp-iter b (* acc acc) (/ n 2)))
	 (else (fast-exp-iter b (* acc b) (- n 1)))))
  (fast-exp-iter b b n))

(define (even n)
  (= (remainder n 2) 0))

(fast-exp 4 2)
