(define (fixed-point f x epsilon)

  (let ((current-term (f x))
	(next-term (f (f x))))
    (newline)
    (display " x "  'x' "f(" 'x' ")=" 'current-term' )
    (newline)
    (cond ((< (abs (- current-term next-term)) epsilon)
	  next-term)
	  (else (fixed-point f current-term epsilon)))))



(define (log100-div-logx x epsilon)
  (fixed-point (lambda (x) (/ (log 1000) (log x)))
	       x
	       epsilon))

(define (log100-div-logx-average-dampening x epsilon)
  (fixed-point (lambda (y) (* 0.5 (+ y (/ (log 1000) (log y)))))
	       x
	       epsilon))

(log100-div-logx 1.1 0.00001)
(log100-div-logx-average-dampening 1.1 0.00001)
