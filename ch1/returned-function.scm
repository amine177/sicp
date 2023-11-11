(define (continued-fraction-iterative n d k)
  (define (cont-frac-iter i total)
    (cond ((= i 0)
	 total)
	(else
	 (cont-frac-iter (- i 1) (/ (n i) (+ (d i) total))))))
  (cont-frac-iter k (/ (n k) (d k))))

(define (fixed-point f x epsilon)
  (let ((current (f x))
	(next (f (f x))))
  (if (< (abs (- current next)) epsilon)
      next
      (fixed-point f current epsilon))))


(define (average a b)
  (/ (+ a b) 2.0))

(define (average-damp f)
  (lambda(x) (average x (f x))))


(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
	       1.0
	       0.000001))


(define (cbrt x)
  (fixed-point (average-damp (lambda (y) (/ x (* y y))))
	       5
	       0.000000001))

(sqrt 9)
(cbrt 2)
