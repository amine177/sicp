(define (* a b)
  (define (double a)
    (+ a a))
  (define (half a)
    (/ a 2))
  (cond
   ((= b 0) 0)
   ((= (remainder b 2) 0)
    (double (* a (half b))))
   (else
    (+ a (* a (- b 1))))))


(* 2 4)
(* 3 2)
(* 15 3)

