(define (find-zero f a b epsilon)
  (let ((mid (/ (+ b a) 2.0)))
    (cond ((< (- b a) epsilon) mid)
	  (let ((mid-image (f mid)))
	  (cond ((> mid-image 0) (find-zero f a mid epsilon))
		((< mid-image 0) (find-zero f mid b epsilon))
		(else mid))))))

(define (half-interval f a b epsilon)
  (let ((a-image (f a))
	(b-image (f b)))
    (cond ((and (> a-image 0) (< b-image 0))
	   (find-zero f b a epsilon))
	  ((and (< a-image 0) (> b-image 0))
	   (find-zero f a b epsilon))
	  (else
	   (error "(f " 'a' ")=" 'a-image' ", (f " b ")=" 'b-image'
		  "are not of opposite signs")))))
	       
	       




(define (square-minus-9 x)
  (- (* x x) 9))

(half-interval square-minus-9 2 2.5 0.00000000000000001)
