(define (fixed-point f x epsilon)
  (let ((current (f x))
	(next (f (f x))))
  (if (< (abs (- current next)) epsilon)
      next
      (fixed-point f current epsilon))))


(fixed-point cos 1.0 0.00000001)
(define (sqrt x)
  (fixed-point (lambda (y) (* 0.5 (+  y (/ x y)))) 1.0 0.000001))
(sqrt 2)
