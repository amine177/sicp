(define (deriv g)
  (let ((dx 0.000001))
    (lambda (x) (/ (- (g (+ x dx))  (g x)) dx))))


(define (square x)
  (* x x))
  


(define (fixed-point f x epsilon)
  (let ((current (f x))
	(next (f (f x))))
  (if (< (abs (- current next)) epsilon)
      next
      (fixed-point f current epsilon))))

(define (newton-method g initial-guess)
  (fixed-point
   (lambda (x) (- x (/ (g x) ((deriv g) x)))) initial-guess 0.0001)) ; find x such as g(x) = 0 therefore
; (lambda(x) ... ) = x : a fixed point of the lambda expression in sense


(define (sqrt x)
  (newton-method
   (lambda (y) (- (square y) x)) 1.0))

(sqrt 2)

(define (cube x)
  (* x x x))


(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(define (solve-cubic a b c)
  (newton-method
   (cubic a b c) 1.0))


(solve-cubic 1 100 1)

