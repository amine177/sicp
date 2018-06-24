(define x 5)

(define (square x)
  (* x x))

(define (sum-of-squares a)
  (+
    (square (+ a 1))
    (square (+ a 2))))

(square x)
(+ x x)
(sum-of-squares x)
