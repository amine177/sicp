(define (deriv g)
  (let ((dx 0.0001))
    (lambda (x) (/ (- (g (+ dx x)) (g x)) dx))))

(define (fixed-point f x epsilon)
  (let ((current (f x))
	(next (f (f x))))
  (if (< (abs (- current next)) epsilon)
      next
      (fixed-point f current epsilon))))


(define (square x) (* x x))

((deriv square) 5)

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newton-method g initial-guess)
  (let ((epsilon 0.00001))
    (fixed-point (newton-transform g) initial-guess epsilon)))


(define (sqrt x)
  (let ((initial-value 1.0))
    (newton-method (lambda (y) (- (* y y) x)) initial-value)))
