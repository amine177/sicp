(define (∂x f y dx)
  (lambda (x) (/ (- (f x y) (f (+ x dx y))) dx)))

(define (∂y f x dy)
  (lambda (y) (/ (- (f x y) (f x (+ y dy))) dy)))

(define (display-interval   interval)
  (newline)
  (display " [" )
  (display (lower-bound interval))
  (display ", ")
  (display (upper-bound interval))
  (display "]")
  (newline))
	   

; Hessian matrix
(define (is-function-monotone? f
			       interval-x
			       interval-y
			       epsilon)
  (define start-sign (cond ((< (f (upper-bound interval-x)
				   (upper-bound interval-y))
				(f (lower-bound interval-x)
				   (lower-bound interval-y))) -1)
			   (else 1)))
  (newline)
  (display start-sign)
  (newline)
  (define (recurse f interval-x interval-y start-sign epsilon)
   ; (newline)
    ;(display start-sign)
    ;(newline)
    (let ((xl (lower-bound interval-x))
	(xu (upper-bound interval-x))
	(yl (lower-bound interval-y))
	(yu (upper-bound interval-y)))
      (display (or
		(and (< (f xu yu) (f xl yl)) (= start-sign 1))
		(and (> (f xu yu) (f xl yl)) (= start-sign -1))))
	(cond ((or
		(and (< (f xu yu) (f xl yl)) (= start-sign 1))
		(and (> (f xu yu) (f xl yl)) (= start-sign -1)))
	       #f)
	      (else
	       (cond ((and
			(close-enough?
			 interval-x
			 epsilon)
			(close-enough?
			 interval-y
			 epsilon))
		
		      #t)
		     ((and
		       (recurse f
				(bisect-interval interval-x #t)
				(bisect-interval interval-y #t)
				start-sign
				epsilon)
		       (recurse f
				(bisect-interval interval-x #t)
				(bisect-interval interval-y #f)
				start-sign
				epsilon)
		       (recurse f
				(bisect-interval interval-x #f)
				(bisect-interval interval-y #t)
				start-sign
				epsilon)
		       (recurse f
				(bisect-interval interval-x #f)
				(bisect-interval interval-y #f)
				start-sign
				epsilon))
		      #t)
		     (else #f))))))
  (recurse f interval-x interval-y start-sign epsilon))

		

				       
				      

(define (close-enough? interval epsilon)
  (< (abs (width interval)) epsilon))

(define (fixed-point-x f x epsilon)
  (cond ((= (f x) x) x)
	((close-enough? (f x) x epsilon) (f x))
	(else (fixed-point f (f x) epsilon))))
(define (make-interval a b)
  (cons a b))

(define (upper-bound x)
  (max (car x) (cdr x)))

(define (lower-bound x)
  (min (car x) (cdr x)))

(define (bisect-interval interval right)
  (cond (right
	 (make-interval (upper-bound interval)
			(/ (+
			    (upper-bound interval)
			    (lower-bound interval)) 2.0)))
	(else
	 (make-interval (lower-bound interval)
			(/ (+
			    (upper-bound interval)
			    (lower-bound interval)) 2.0)))))
	 
(define (mul-interval x y)
  (let ((ll (* (lower-bound x) (lower-bound y)))
	(lu (* (lower-bound x) (upper-bound y)))
	(ul (* (upper-bound x) (lower-bound y)))
	(uu (* (upper-bound x) (upper-bound y))))
    (make-interval (min ll lu ul uu)
		   (max ll lu ul uu))))

(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2.))

(define (div-interval x y)
  (cond ((= (width y) 0)
	 (error "can't divide by an interval of width 0")))
; the old implementation inverts lower and upper bound
  (mul-interval x
	       (make-interval (/ 1.0 (upper-bound y))
			      (/ 1.0 (lower-bound y)))))

	
(define (par2 r1 r2)
  (let ((unit (make-interval 1.0 1.0)))
    (div-interval unit
		  (add-interval (div-interval unit r1)
				(div-interval unit r2)))))

(define (par2_scalar r1 r2)
  (/ 1.0 (+ (/ 1.0 r1) (/ 1.0 r2))))


(define (sin_multi x y)
  (+ (sin x) (sin y)))

(is-function-monotone? sin_multi
		       (make-interval 1 20)
		       (make-interval 2 40)
		       0.001)
