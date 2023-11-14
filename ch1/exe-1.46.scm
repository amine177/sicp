(define (iterative-improve predicate improve)
  (define (iter guess)
    (if (predicate guess (improve guess))
	(improve guess)
	(iter (improve guess))))
  (lambda (x)
    (trace iter)
    (iter x)))






(define (sqrt x)
  ((iterative-improve  (lambda (a b) (< (abs (- a b)) 0.001))
		       (lambda (guess) (average  guess (/ x guess))))
		       1.0))






(define (fixed-point f x epsilon)
  ((iterative-improve
    (lambda (a b) (< (abs (- a b)) epsilon))
    (lambda (guess) (f guess))) x))
