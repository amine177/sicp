 (define (generic-sum f a b iter)
  (if (> a b) 0
      (+ (f a) (generic-sum  f (iter a) b iter))))

(define (generic-op f g a b iter)
  (if (> a b) 0
      (g (f a) (generic-op f g (iter a) b iter))))

(define (generic-combiner f g end-predicate a b iter neutral-element)
  (if (end-pridicate a b) neutral-element
      (g (f a) (generic-combiner (iter a) b iter neutral-element))))


(define (cube x)
  (* x x x))

(define (square x)
  (* x x))

(define (integral f a b dx)
  (define (next-term x)
    (+ x dx))
  (* (generic-sum f  (+ a (/ dx 2.0)) b next-term) dx))


(integral cube 0 1 0.000001)
(integral square 0 1 0.01)
