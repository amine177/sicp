(define (half b)
  (/ b 2))
(define (even? b)
  (= (remainder b 2) 0))
(define (double b)
  (+ b b))
(define (* a b)
  (define (prod-internal a b res)
    (cond
     ((= b 0) res)
     ((even? b) (prod-internal (double a) (half b) res))
     (else (prod-internal a (- b 1) (+ res a)))))
  (prod-internal a b 0))


(* 2 4)
(* 10 10)
