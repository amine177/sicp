(define (fixed-point f x epsilon)
  (let ((current (f x))
	(next (f (f x))))
  (if (< (abs (- current next)) epsilon)
      next
      (fixed-point f current epsilon))))

(define (golden-ratio x epsilon)
  (fixed-point
;   (lambda (x) (+ 1 (/ 1 x)))
   (lambda (y) (* 0.5 (+ y (+ 1 (/ 1 y))))) ; with dampening
	 x
	 epsilon))


(golden-ratio 1.0 0.0001)
