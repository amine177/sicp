(define (product f a b next)
  (if (> a b)
      1
      (* (f a) (product f (next a) b next))))

(define (product-iterative f a b next total)
  (if (> a b)
      total
      (product-iterative f (next a) b next (* total (f a)))))

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


(define (pi-iter n)
  (define (fraction p)
    (* (/ p (- p 1)) (/ p (+ p 1))))
    (define (next x)
      (+ x 2))
    (* 2 (product-iterative fraction 2.0 n next 1)))


(pi 12.0)
(pi-iter 10.0)
