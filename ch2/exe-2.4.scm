(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (x y) x)))

(define (cdr z)
  (z (lambda (x y) y)))


(car (cons 1 2))

(cdr (cons 1 2)) ; (cdr (lambda (m) (m 1 2)))
		; ((lambda (m) (m 1 2)) (lambda (x y) y))
		; ((lambda (x y) y) 1 2)
		; Value: 2



  
