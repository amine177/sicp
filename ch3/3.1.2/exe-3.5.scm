(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (random-in-range low high)
     (let ((range (- (* high 1.0) low)))
       (+ low (random range))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (monte-carlo trials 
	       (lambda ()
			(P  x1 x2 y1 y2))))
(define (estimate-pi-integral trials)
  (estimate-integral (lambda (x1 x2 y1 y2)
		       (circle-area-test 1 2 2 x1 x2 y1 y2)) 1 3 1 3 trials))
(trace estimate-integral)
  

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (circle-area-test ray center-x center-y x1 x2 y1 y2)
  (let ((rand_x (random-in-range x1 x2))
	(rand_y (random-in-range y1 y2)))
    (<= (+ (square (- rand_x center-x))
	   (square (- rand_y center-y))) (square ray))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
	   (/ trials-passed trials))
	  ((experiment)
	   (iter (- trials-remaining 1)
		 (+ trials-passed 1)))
	  (else
	   (iter (- trials-remaining 1)
		 trials-passed))))
  (iter trials 0))

(estimate-pi 300)
(exact->inexact (* 4 (estimate-pi-integral 100))) ; i am not sure why i need (* 4 x)
