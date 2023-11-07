 (define (sum f a b next)
  (if (> a b) 0
  (+ (f a) (sum f (next a) b next))))


(define (simpson-rule f a b n)
  (define h (/ (- b a) n))
  (define (next x)
    (+ x (* 2 h)))
  (define (g x)
    (+ (* 4 (f (+ x h))) (* 2 (f (+ x (* 2 h))))))
  (* (/ h 3)
     (+
      (sum g (next a) b next)
      (f a)
      (f b))))

(define (integral f a b dx)
  (define (next-term x)
    (+ x dx))
  (* (sum f  (+ a (/ dx 2.0)) b next-term) dx))


(define (cube x)
  (* x x x))
(simpson-rule  cube 0 1 10000.0)
(integral  cube 0 1 0.0001)
