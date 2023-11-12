(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeat f n)
  (define (iter comp k)
       (cond ((< k 2)
	      comp)
	     (else
	      (iter (compose f comp ) (- k 1)))))
  (iter f n))

(define (fixed-point f x)
  (let ((epsilon 0.0001)
	(current (f x))
	(next ((compose f f) x)))
    (cond ((< (abs (- current next)) epsilon)
	   x)
	  (else (fixed-point f current)))))

(define (average x y)
  (/ (+ x y) 2.0))

(define (average-dump f)
  (lambda (x) (average x (f x))))

(define (fast-exp b n)
  (cond ((= n 0) 1)
	((iseven n)
	 (* (fast-exp b (/ n 2)) (fast-exp b (/ n 2))))
	(else (* b (fast-exp b (- n 1))))))

(define (iseven n)
  (= (remainder n 2) 0))

(define (n-th-squre  x n)
  (fixed-point
   (repeat
     (average-dump (lambda (y) (/ x (fast-exp y (- n 1)))))
     (- n 1))
   1.0)) ; initial guess 1.0
