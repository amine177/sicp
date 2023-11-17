(define (iseven n)
  (= (remainder n 2) 0))

(define (fast-exp b n)
  (cond ((= n 0) 1)
	((iseven n)
	 (* (fast-exp b (/ n 2)) (fast-exp b (/ n 2))))
	(else (* b (fast-exp b (- n 1))))))

(define (cons x y)
  (* (fast-exp 2 x) (fast-exp 3 y)))

(define (car z)
  (define (iter n count)
    (cond ((= (remainder n 2) 0)
	   (iter (/ n 2) (+ 1 count)))
	  (else count)))
  (iter z 0))

(define (cdr z)
  (define (iter n count)
    (cond ((= (remainder n 3) 0)
	   (iter (/ n 3) (+ 1 count)))
	  (else count)))
  (iter z 0))
