(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (cond ((< n 2)
      f)
      (else
       (repeated (compose f f ) (- n 1)))))

(define (square x)
  (* x x))
((repeated square 2) 5)
