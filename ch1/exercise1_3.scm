(define (square a)
  (* a a))

(define (diagonal a b c)
  (cond
    ((and (> a b) (> b c))
             (+ (square a) (square b)))
    ((and (> b a) (> c a))
             (+ (square b) (square c)))
    (else
      (+ (square  c) (square a)))))

(diagonal 5 3 1)

