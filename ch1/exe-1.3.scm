(define (square x)
  (* x x))

(define (sum-of-squares a b)
  (+
    (square a)
    (square b)))

(define (sum-largest a b c)
  (cond ((> a b)
	 (cond ((> b c) (sum-of-squares a b))
	       (else (sum-of-squares a c))))
	((> a c) (sum-of-squares a b))
	(else (sum-of-squares b c))))
