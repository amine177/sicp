(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (epsilon ) 0.000000001)

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) (epsilon)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))



(sqrt-iter 1.0 9)
(sqrt-iter 1.0 2)
