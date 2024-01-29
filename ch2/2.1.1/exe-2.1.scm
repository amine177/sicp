(define (gcd x y)
  (cond ((= y 0)
	 x)
	(else (gcd y (remainder x y)))))

(define (sign x y)
  (/ (* x y) (abs (* x y))))

(define (make-rational x y)
  (let ((rational-gcd (gcd (abs x)  (abs y)))
	(number-sign (sign x y)))
    (newline)
    (display (sign x y))
    (newline)
    (cond ((> number-sign 0)
	   (cons
	    (/(abs x) rational-gcd)
	   (/ (abs y) rational-gcd)))
	  (else
	   (cons
	    (/ (* -1 (abs x)) rational-gcd)
	   (/ (abs y) rational-gcd))))))
