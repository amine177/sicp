
      


(define (calculate_next x_n x)		; x_n+1 = x_n - f(x_n) / f'(x_n)
					; f(x) = x^2 - y , solve for f(x) = 0
 					;   x_n+1 = x_n - (x_n^2 - 2) / 2*(x_n)

  (- x_n (/ (- (* x_n x_n) 2) (* 2 x_n))))
(define (abs a)
  (if (< a 0) (- a)
      a))
(define (sqrt-iter x x_n epsilon)
  (if (<  (abs (-  (* x_n x_n) x))  epsilon)
      x_n
      (sqrt-iter x (calculate_next x_n x) epsilon)))
(define (sqrt x  x_0 epsilon)
  (sqrt-iter x x_0 epsilon))
(sqrt 2 1 0.00000000000001)
