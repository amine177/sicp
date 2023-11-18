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
  (cond ((= (width y) 0)
	 (error "can't divide by an interval of width 0")))
  (mul-inerval x
	       (make-interval (/ 1.0 (upper-bound y))
			      (/ 1.0 (lower-bound y)))))

(define (make-interval a b)
  (cons a b))

(define (upper-bound x)
  (max (car x) (cdr x)))

(define (lower-bound x)
  (min (car x) (cdr x)))

(define (sub-interval x y)
  (add-interval x
		(make-interval
		 (- (lower-bound y))
		 (- (upper-bound y)))))

(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))
