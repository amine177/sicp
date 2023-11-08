(define (find-zero f a b epsilon)
  (let ((mid (/ (+ b a) 2.0)))
    (cond ((< (- b a) epsilon) mid)
	  (let ((mid-image (f mid)))
	  (cond ((> mid-image 0) (find-zero f a mid epsilon))
		((< mid-image 0) (find-zero f mid b epsilon))
		(else mid))))))

	       
	       




(define (square-minus-9 x)
  (- (* x x) 9))

(find-zero square-minus-9 2 5 0.00000000000000001)
