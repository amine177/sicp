(define (product f a b next)
  (if (> a b)
      1
      (* (f a) (product f (next a) b next))))

(define (factorial n)
  (define (next x)
    (+ x 1))
  (define (identity x)
    x)
  (product identity 1 n next))

(factorial 5)


(define (pi n)
  (define (fraction p)
    (* (/ p (- p 1)) (/ p (+ p 1))))
  (define (next x)
    (+ x 2))
  (* 2 (product fraction 2.0 n next)))


(pi 10000.0)
