(define (add-interval interval1 interval2)
  (make-interval (+ (lower-bound interval1) (lower-bound interval2))
		 (+ (upper-bound interval1) (upper-bound interval2))))

(define (mul-interval x y)
  (let ((ll (* (lower-bound x) (* lower-bound y)))
	(lu (* (lower-bound x) (* upper-bound y)))
	(ul (* (upper-bound x) (* lower-bound y)))
	(uu (* (upper-bound x) (* upper-bound y))))
    (make-interval (min ll lu ul uu)
		   (max ll lu ul uu))))


(define (div-interval x y)
  (mul-inerval x
	       (make-interval (/ 1.0 (upper-bound y))
			      (/ 1.0 (lower-bound y)))))
