(define (abs x)
  (cond ((< x 0) (- x))
	(x)))

(define (square x)
  (* x x))

(define (sqrt x)
  (define (good? x_n)
    (< (abs (- x (* x_n x_n))) 0.001))
  (define (refine x_n)
    (/ (+ x_n (/ x x_n)) 2))
  (define (sqrt-it x_n)
    (if (good? x_n)
	x_n
	(sqrt-it (refine x_n))))
  (sqrt-it 1.0))
