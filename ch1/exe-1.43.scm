(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (iter comp k)
       (cond ((< k 2)
	      comp)
	     (else
	      (iter (compose f comp ) (- k 1)))))
  (iter f n))

(define (square x)
  (* x x))
((repeated square 2) 5)
