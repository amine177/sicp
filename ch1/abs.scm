 (define (abs x)
  (cond ((> x 0) x)
	(else (- x))))

(define (abs-if x)
  (if (or (< x 0) (= x 0))
      (- x)
      x))

(abs -5)
(abs-if -10)
(abs 5)
