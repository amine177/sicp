(define (even x)
  (= (bitwise-and x 1) 0))

(define (e-m a pwr n)
  (cond
   ((= pwr 0) 1)
   ((even pwr) (remainder (e-m (* a a) (/ pwr 2) n) n))
   (else (remainder (* a (e-m a (- pwr 1) n)) n))))


(define (fermat-test n a  treshold)
  (trace e-m)
  (cond
   ((= treshold 0) true)
   ((= (e-m a n n) a) (fermat-test n (+ 1  (random (- n 1)))
				   (- treshold 1)))
   (else false)))

