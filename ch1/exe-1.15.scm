(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cude x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))



; x / 3^n = 10^-1 => n = Floor((log(x) + log(10))/log(3))+ 1 ,
; for x = 12, n = 5
; O(log(x)) for both space and time
