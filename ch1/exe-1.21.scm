(define (find-smallest-divisor n)
  (define (f-s-d n d)
    (cond ((> (* d d) n)
	   n)
	  ((= (remainder n d) 0)
	     d)
	  (else
	   (f-s-d n (+ d 1)))))
  (trace f-s-d)
  (f-s-d n 2))
(find-smallest-divisor 199)
(find-smallest-divisor 199)
(find-smallest-divisor 1999)
(find-smallest-divisor 19999)
