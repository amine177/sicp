(define (scube-iter y prev x epsilon)
  (if (good y prev epsilon)
      y
      (scube-iter (refine y x) y x epsilon)))

(define (good y prev epsilon)
  (if (< (abs (- y prev))  epsilon) ; is the estimation changing slowly ?
      true ; if so, we are so close to an accepted estimation
      false))

(define (refine y x)
  (/ (+ (/ x (* y y)) (+ y y)) 3.)) ; newton's method

(define (abs x)
  (cond ((< x 0) (- x))
	(else x)))

(define (refine-epsilon x eps)
  (if (<= x eps)
      (refine-epsilon x (/ eps 10))
      eps))

(define (scube x epsilon)
  (scube-iter x_0 0 x (refine-epsilon  x epsilon)))

(define x_0 1)

(scube 8 0.001)

