; The error of margin on interval arithmetic
; comes from the fact that the functions that
; we apply on them might be convex (or concave).
; This class of functions exhibits
; a minimum or a maximum not at the extremeties
; of the intervals but within them.
; Finding those minimums and maximums
; can be done with a variation of gradient
; descent and other numerical methods.
; This task is not trivial given the limited
; tools we have been given by the previous chapters.
; We tried to implement a naive sketch of solution
; using a stricit monotonity check and
; introduced a gradient descent and a newton method
; for multivariate functions.
; We are not sure if the implementation is correct
; We might try to improve on it later.



(define (dfdx f dx)
  
  (lambda (x y) (/ (- (f (+ x dx) y) (f x y)) dx)))

(define (dfdy f dy)
  (lambda (x y) (/ (- (f x (+ y dy)) (f x y )) dy)))

(define (dfdxdx f dx)
  (dfdx (dfdx f dx) dx))

(define (dfdydy f dy)
  (dfdy (dfdy f dy) dy))

n(define (dfdydx f dx dy)
  (dfdy (dfdx f dx) dy))

(define (dfdxdy f dx dy)
  (dfdx (dfdy f dy) dx))

(define (grad-f f dx dy)
  (lambda (x y) (cons ((dfdx f dx) x y)
		      ((dfdy f dy) x y))))

(define (display-interval   interval)
  (newline)
  (display " [" )
  (display (lower-bound interval))
  (display ", ")
  (display (upper-bound interval))
  (display "]")
  (newline))

; Monotonity check
(define (is-function-monotone? f
			       interval-x
			       interval-y
			       epsilon)
  (define start-sign (cond ((< (f (upper-bound interval-x)
				   (upper-bound interval-y))
				(f (lower-bound interval-x)
				   (lower-bound interval-y))) -1)
			   (else 1)))

  (define (recurse f interval-x interval-y start-sign epsilon)
    ;(newline)
    ;(display interval-x)
    ;(newline)
    (let ((xl (lower-bound interval-x))
	(xu (upper-bound interval-x))
	(yl (lower-bound interval-y))
	(yu (upper-bound interval-y)))
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


(define (sin_multi_scalar x y)
  (+ (sin x) (sin y)))

(define (cos_multi_scalar x y)
  (+ (cos x) (cos y)))



(define (e_scalar x y)
  (* (fast-exp 1.6 x) (fast-exp 1.6 y)))

(is-function-monotone? sin_multi_scalar
		       (make-interval 1 20)
		       (make-interval 2 40)
		       0.001)


(is-function-monotone? par2_scalar
		       (make-interval 1 20)
		       (make-interval 2 40)
		       0.1)

(is-function-monotone? cos_multi_scalar
		       (make-interval 1 20)
		       (make-interval 2 40)
		       0.1)

((dfdxdx (lambda (x y) (+ x y)) 0.00001) 1 1)

(define (hessian-1-f f dx dy)
  	  
	      
  (lambda (x y)
    (let* ((dxx ((dfdxdx f dx) x y))
	   (dyy ((dfdydy f dy) x y))
	   (dyx ((dfdydx f dx dy) x y))
	   (dxy ((dfdxdy f dx dy) x y))
	   (det (/ 1 (- (* dxx dyy) (* dyx dxy)))))
;      (newline)
;      (display det)
;      (newline)
      
;      (if (and (< dxx 0.000000000000000001)
;	       (< dyy 0.000000000000000001)
;	       (< dyx 0.000000000000000001)
;	       (< dxy 0.000000000000000001))
					;	  (cons (cons 0 0) (cons 0 0))
      ;(newline)
      ;(display det)
      ;(newline)
	  (cons
	   (cons (* dyy det)
		 (* dxy det))		 
	   (cons (* dyx det)
		 (* dxx det))))))

(define hcos ((hessian-1-f cos_multi_scalar  0.0000001 0.0000001) 1 1))
(define gradcos ((grad-f cos_multi_scalar 0.0000001 0.0000001) 1 1))

(define (mul-hessian-gradient hessian gradient)
;  (newline)
 ; (display (car gradient))
  ;(newline)
  (cons (+
	 (* (car (car hessian)) (car gradient))
	 (* (cdr (car hessian)) (cdr gradient)))
	(+
	 (* (car (cdr hessian)) (car gradient))
	 (* (cdr (cdr hessian)) (cdr gradient)))))

(define (sum-vectors v1 v2)
  (cons (+ (car v1) (car v2))
	(+ (cdr v1) (cdr v2))))

(define (good-enough? pt1 pt2 epsilon)
  (and (< (abs (- (car pt1) (car pt2))) epsilon)
       (< (abs (- (cdr pt1) (cdr pt2))) epsilon)))

(define (newton-two-variables
	 f
	 current_point
	 driv-epsilon
	 pt-epsilon
	 max-recurse)
;  (newline)
;  (display current_point)
					;  (newline)
  (let* ((h-1 ((hessian-1-f  f
			     driv-epsilon
			     driv-epsilon)
	       (car current_point)
	       (cdr current_point)))
	 (grad ((grad-f  f
					       driv-epsilon
					       driv-epsilon)
	       (car current_point)
	       (cdr current_point)))
	 (next-pt (sum-vectors current_point
				(mul-hessian-gradient
				 h-1
				 grad
				 ))))
    
    ;(display "******************")
    ;(newline)
    ;(display "current pt ")
    ;(display current_point)
    ;(newline)
    ;(display "grad ")
    ;(display grad)
    ;(newline)
    ;(display "hessian ")
    ;(display h-1)
    ;(newline)
    ;(display "**************")
    ;(newline)
    (if (= max-recurse 0)
	next-pt)
    (if (good-enough? current_point next-pt pt-epsilon)
	next-pt
	(newton-two-variables
	 f
	 next-pt
	 driv-epsilon
	 pt-epsilon
	 (- max-recurse 1)))))

(define (gradient-descent
	 f
	 current-pt
	 epsilon
	 x-interval
	 y-interval
	 driv-epsilon
	 descent-ratio)
  (let* ((grad ((grad-f  f
			 driv-epsilon
			 driv-epsilon)
		(car current_point)
		(cdr current_point)))
	 (next-pt (sum-vectors current_point
			       (cons (-
				      (* descent-ratio
					 (car grad)))
				     (-
				      (* descent-ratio
					 (- (cdr grad))))))))
    (if (out-of-intervals? next-pt x-interval y-interavl)
	(error "gradient descent diverged from given interval")
	(if (close-enough? current-pt next-pt)
		  (next-pt)
		  (gradient-descent f
				    next-pt
				    x-interval
				    y-interavl
				    driv-epsilon
				    descent-ratio)))))
			  
					       
				 

(define (add-pts pt1 pt2)
  (cons (+ (car pt1) (car pt2))
	(+ (cdr pt1) (cdr pt2))))

					; this will converge to the closest special point in
					; a descending manner
(if (not (is-function-monotone? cos_multi_scalar
				(cons 2 30)
				(cons 1 20)
				0.01))
    (newton-two-variables
     cos_multi_scalar
     (cons 2 1)
     0.000001
     0.001
     100000)
    (display "function is monotone, so f(R1, R2) has no error margin"))
;(is-function-monotone? cos_multi_scalar (cons 1 2) (cons 1 2) 0.0001)
