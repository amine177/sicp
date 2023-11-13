(define (make-rational x y)
  (cons x y))

(define (numerator rational-number)
  (car rational-number))

(define (denominator rational-number)
  (cdr rational-number))

(define (print-rational x)
  (newline)
  (display (numerator x))
  (display "/")
  (display (denominator x))
  (newline))


(define 2-by-5 (make-rational 2 5))
(print-rational 2-by-5)
