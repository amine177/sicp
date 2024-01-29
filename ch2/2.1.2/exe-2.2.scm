(define (iterative-improve predicate improve)
  (define (iter guess)
    (if (predicate guess (improve guess))
	(improve guess)
	(iter (improve guess))))
  (lambda (x)
    (iter x)))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  ((iterative-improve  (lambda (a b) (< (abs (- a b)) 0.001))
		       (lambda (guess) (average  guess (/ x guess))))
   1.0))

(define (make-segment start-pt end-pt)
  (cons start-pt end-pt))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (point x y)
  (cons x y))

(define (x-point pt)
  (car pt))

(define (y-point pt)
  (cdr pt))

(define (midpoint-segment segment)
  (let ((start-pt (start-segment segment))
	(end-pt (end-segment segment)))
    (let (
	  (mid-x (/ (+ (x-point start-pt) (x-point end-pt)) 2))
	  (mid-y (/ (+ (y-point start-pt) (y-point end-pt)) 2)))
      (point mid-x mid-y))))


(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))


(let ((segment (make-segment (point 1 2) (point 5 (sqrt 5)))))
  (print-point (midpoint-segment segment)))
