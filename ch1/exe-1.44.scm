(define (compose f g)
  (lambda (x) (f (g x))))


(define (repeat f n)
  (define (iter comp k)
       (cond ((< k 2)
	      comp)
	     (else
	      (iter (compose f comp ) (- k 1)))))
  (iter f n))

; define the smooth procedure
(define (smooth-once f)
  (let ((dx  0.001))
    (lambda (x) (/ (+ (f (- x dx))  (f x) (f (+ x dx))) 3))))

; smooth n-time fofo...f , f^n
(define (nfold-smooth f n)
  (repeat (smooth-once f) n))

((nfold-smooth (lambda (x) (+ x 1)) 1) 1)
((nfold-smooth (lambda (x) (+ x 1)) 2) 1)
((nfold-smooth (lambda (x) (+ x 1)) 3) 1)
((nfold-smooth (lambda (x) (+ x 1)) 4) 1)
