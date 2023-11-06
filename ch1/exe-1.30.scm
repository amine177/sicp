(define (sum term a b next)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ result (term a)))))
  (iter a 0))



(define (integral f a b dx)
  (define (next-term x)
    (+ x dx))
  (* (sum f  (+ a (/ dx 2.0)) b next-term) dx))


(define (cube x)
  (* x x x))

(integral  cube 0 1 0.0001)
