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
  ((iterative-improve  (lambda (a b) (< (abs (- a b)) 0.00001))
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

(define (first-point segment)
  (car segment))

(define (second-point segment)
  (cdr segment))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (print-segment segment)
  (display "segment = [Point1: ")
  (print-point (car segment))
  (display ", Point2: ")
  (print-point (cdr segment))
  (display "]"))
  

(define (segment-length segment)
  (let ((pt1 (car segment))
	(pt2 (cdr segment)))
    (sqrt (+ (square (abs (- (x-point pt1)
			     (x-point pt2))))
	     (square
		     (abs (- (y-point pt1)
			     (y-point pt2)))))))))

(let ((segment (make-segment (point 1 2) (point 5 (sqrt 5)))))
  (print-point (midpoint-segment segment)))

(define (rectangle seg1 seg2 seg3 seg4)
(cons (cons seg1 seg2) (cons seg3 seg4))) ; length, height;
					;length, height


(define (rectangle-length rectangle)
(segment-length (car (car rectangle))))

(define (rectangle-height rectangle)
(segment-length (cdr (car rectangle))))

(define (rectangle-area rectangle)
  (let ((length (rectangle-length rectangle))
	(height (rectangle-height rectangle)))
    (newline)
    (display "length: ")
    (display length)
    (newline)
    (display "height: ")
    (display height)
    (newline)

    (* length height)))


					; the 2nd way of defining
					; a rectangle would be by
					;using length and height
					;numerals
