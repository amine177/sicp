					; when x is too small it will fall under the error margin, so
					; the execution will stop from the first iteration
					; when x is too big, the difference between x and the guess will be always bigger then the error margin, therefore (good ) will always return false -> infinite loop
					; the solution for the small x is to make epsilon small enough
; the solution for the big x is always compare a current guess against the previous guess and if guess doesn't change less than a treshold halt
(define (refine lower upper)
	(/ (+ lower (/ upper lower)) 2.0))				; newton's method x_n+1 = x_n - f(x_n) / f'(x_n)

(define (refine-epsilon epsilon x)
  (if (<= x epsilon) (refine-epsilon (/ epsilon 10) x)
      epsilon))


(define (abs a)
  (if (< a 0) (- a)
      a))

(define (good y prev approx)
  (if (< (abs (- y prev)) (* y approx)) true
      false))

(define (sqrt-next x_n prev x approx)
  (if (good x_n prev (refine-epsilon approx x)) ; predicate
      x_n          ; consequent
      (sqrt-next (refine x_n x) x_n x (refine-epsilon approx x))))

(define x_0 1)

(define (sqrt x approx)
  (if (= x 0) 0
  (sqrt-next x_0 0 x approx)))

(define epsilon 0.00001) ; approximation value
(sqrt 2 epsilon)
(sqrt 9 epsilon)
