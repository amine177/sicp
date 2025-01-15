(define (make-accumulator amount)
  (define (dispatch addend)
    (begin (set! amount (+ amount addend))
	   amount))
  dispatch)

; using fold, just keeping it here to build the habit
(define (make-accumulator-fold amount)
  (define (dispatch addend)
    (begin (set! amount (fold-right + amount (list addend)))
	   amount))
  dispatch)

(define A (make-accumulator 10))
(A 5)
(A 10)
