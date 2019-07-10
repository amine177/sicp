(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (epsilon ) 0.000000001)

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) (epsilon)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))



(sqrt-iter 1.0 9)
(sqrt-iter 1.0 2)

; because new-if is a compound procedure, its parameters
; will get evaluated  befaure applying it, which means
; another expansion of sqrt-iter , which by itself
; will expand to another call to new-if ... until reaching
; the recursion's maximum depth limit
