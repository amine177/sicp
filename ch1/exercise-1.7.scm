(define (refine lower upper)
  (/ (+ lower (/ upper lower)) 2.0)) ; newton's method x_n+1 = x_n - f(x_n) / f'(x_n)

(define (refine-epsilon epsilon x)
  (display "epsilon ")
  (display epsilon)
  (display "\n")
  (if (<= x epsilon) (refine-epsilon (/ epsilon 10) x)
      epsilon))

(define (square a) (* a a))

(define (abs a)
  (if (< a 0) (- a)
      a))

(define (good y x approx)
  (display y )
  (display "\n")
  (if (< (abs (- x (square y))) approx) true
      false))

(define (sqrt-next x_n x approx)
  (if (good x_n x (refine-epsilon approx x)) ; predicate
      x_n          ; consequent
      (sqrt-next (refine x_n x) x (refine-epsilon approx x))))

(define x_0 1)

(define (sqrt x approx)
  (if (= x 0) 0
  (sqrt-next x_0 x approx)))

(define epsilon 0.00001) ; approximation value
(sqrt 2 epsilon)
(sqrt 9 epsilon)
      
