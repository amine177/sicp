(define (refine lower upper)
  (/ (+ lower (/ upper lower)) 2)) ; newton's method x_n+1 = x_n - f(x_n) / f'(x_n)

(define epsilon 0.00001) ; approximation value

(define (square a) (* a a))

(define (abs a)
  (if (< a 0) (- a)
      a))

(define (good y x)
  (if (< (abs (- x (square y))) epsilon) true
      false))

(define (sqrt-next x_n x)
  (if (good x_n x) ; predicate
      x_n          ; consequent
      (sqrt-next (refine x_n x) x)))



      
