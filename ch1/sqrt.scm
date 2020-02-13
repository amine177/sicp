(define (refine lower upper)
  (/ (+ lower (/ upper lower)) 2.0)) ; newton's method x_n+1 = x_n - f(x_n) / f'(x_n)



(define (square a) (* a a))

(define (abs a)
  (if (< a 0) (- a)
      a))

(define (good y x approx)
  (if (< (abs (- x (square y))) approx) true
      false))

(define (sqrt-next x_n x approx)
  (if (good x_n x approx) ; predicate
      x_n          ; consequent
      (sqrt-next (refine x_n x) x approx)))

(define x_0 1)

(define (sqrt x approx)
  (round-by (sqrt-next x_0 x approx)) approx)

(define epsilon 0.00001) ; approximation value
(sqrt 2 epsilon)
(sqrt 9 epsilon)
      
